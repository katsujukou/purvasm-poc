-- Pattern matching compiler
module Purvasm.MiddleEnd.ELambda.PatternMatch where

import Prelude

import Control.Alternative (guard)
import Control.Monad.Reader (class MonadReader, ask)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array (foldr, mapWithIndex, (!!), (..))
import Data.Array as Array
import Data.Array.Partial as ArrayP
import Data.Array.ST as STArray
import Data.Function (on)
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (for, for_, traverse)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Effect.Console (logShow)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PureScript.CoreFn as CF
import Purvasm.MiddleEnd.ELambda.Syntax (ELambda(..))
import Purvasm.MiddleEnd.ELambda.Translate.Env (GlobalEnv(..), TranslEnv, ConstructorDesc)
import Purvasm.MiddleEnd.Types (Arity, AtomicConstant(..), ConstructorTag(..), GlobalName, Ident(..), ModuleName(..), Primitive(..), StructureConstant(..), mkGlobalName)
import Record as Record
import Type.Proxy (Proxy(..))

type Expr = CF.Expr CF.Ann

type Binder = CF.Binder CF.Ann

data Pattern
  = PatWildcard
  | PatVar Ident
  | PatNamed Ident Pattern
  | PatConst AtomicConstant
  | PatConstruct ConstructorDesc GlobalName (Array Pattern)
  -- Array and Record pattern should be eliminated desugaring
  | PatArray (Array Pattern)
  | PatRecord (Array (String /\ Pattern))

derive instance Generic Pattern _
derive instance Eq Pattern
derive instance Ord Pattern
instance Show Pattern where
  show p = genericShow p

binderToPattern :: forall m. MonadReader TranslEnv m => CF.Binder CF.Ann -> m Pattern
binderToPattern = case _ of
  blit@(CF.BinderLit _ lit)
    | Just const <- constBinder blit -> pure (PatConst const)
    | CF.LitArray elems <- lit -> PatArray <$> traverse binderToPattern elems
    | CF.LitRecord props <- lit -> PatRecord <$> for props (traverse binderToPattern >>> map propTuple)
  CF.BinderNamed _ (CF.Ident v) binder -> PatNamed (Ident v) <$> binderToPattern binder
  CF.BinderNull _ -> pure PatWildcard
  CF.BinderVar _ (CF.Ident v) -> pure $ PatVar (Ident v)
  CF.BinderConstructor _ _ (CF.Qualified (Just (CF.ModuleName mn)) (CF.Ident ctor)) args -> do
    { global: GlobalEnv ge } <- ask
    let ctorGlobalName = mkGlobalName (ModuleName mn) (Ident ctor)
    case Map.lookup ctorGlobalName ge.constructors of
      Nothing -> unsafeCrashWith "binderToPattern: Unknown constructor"
      Just desc -> PatConstruct desc ctorGlobalName <$> traverse binderToPattern args
  _ -> unsafeCrashWith "binderToPattern"
  where
  propTuple (CF.Prop k v) = k /\ v

  constBinder :: Binder -> Maybe AtomicConstant
  constBinder = case _ of
    CF.BinderLit _ lit -> case lit of
      CF.LitInt i -> Just (ACInt i)
      CF.LitNumber n -> Just (ACNumber n)
      CF.LitChar c -> Just (ACChar c)
      CF.LitString s -> Just (ACString s)
      CF.LitBoolean b -> Just (ACBoolean b)
      _ -> Nothing
    _ -> Nothing

data DecisionTree
  = Leaf ELambda
  | JumpThru ELambda (Array (Tuple Int DecisionTree))
  | Condition ELambda (Array (Tuple AtomicConstant DecisionTree))
  | Fail
  | TryWith DecisionTree DecisionTree

derive instance Generic DecisionTree _
instance Show DecisionTree where
  show dt = genericShow dt

divideMatching :: PatternMatching -> DecisionTree
divideMatching pm@(PatternMatching caseHeads matrix) = case Array.uncons matrix of
  Nothing -> Fail
  Just { head: { patList, action } }
    | Array.null caseHeads -> Leaf action
    | Just pat <- Array.head patList
    , alwaysMatch pat ->
        let
          vars /\ others = splitVarMatching pm
        in
          combine (divideMatching vars) (divideMatching others)
    | otherwise ->
        let
          nonVars /\ others = splitNonVarMatching pm
          _ = expandMatching nonVars
        in
          combine (Fail) (divideMatching others)
  where
  expandMatching (PatternMatching h mat) = unsafePartial do
    let
      grouped = groupBy (sameToplevelSymbol `on` (_.patList >>> ArrayP.head)) mat
      expanded = grouped <#> \submat -> case ArrayP.head (ArrayP.head submat).patList of
        PatConstruct desc _ _ -> do
          let
            childSlots = xrange 0 desc.arity
            casHead = ArrayP.head h
            expandedHeads = (\i -> ELPrim (PGetField i) [ casHead ]) <$> childSlots
            expandedMatrix =
              ( \ln ->
                  let
                    PatConstruct _ _ patList = ArrayP.head ln.patList
                  in
                    ln { patList = patList <> (Array.drop 1 ln.patList) }
              ) <$> submat
          desc.tag /\ PatternMatching expandedHeads expandedMatrix
      _ = unsafePerformEffect do
        logShow expanded
    Fail

  splitVarMatching :: PatternMatching -> PatternMatching /\ PatternMatching
  splitVarMatching (PatternMatching h mat) = case Array.uncons h of
    Nothing -> unsafeCrashWith "Illegal empty matrix"
    Just { tail: casL } ->
      let
        { init: varsMatrix, rest } = Array.span (_.patList >>> Array.head >>> maybe false alwaysMatch) mat
        vars = PatternMatching casL (map (Record.modify (Proxy @"patList") (Array.drop 1)) varsMatrix)
      in
        Tuple vars (PatternMatching h rest)

  splitNonVarMatching :: PatternMatching -> PatternMatching /\ PatternMatching
  splitNonVarMatching (PatternMatching h mat) =
    let
      { init, rest } = Array.span (_.patList >>> Array.head >>> maybe false (not <<< alwaysMatch)) mat
    in
      Tuple (PatternMatching h init) (PatternMatching h rest)

  combine :: DecisionTree -> DecisionTree -> DecisionTree
  combine = case _, _ of
    Fail, r -> r
    l, Fail -> l
    l, r -> TryWith l r

  alwaysMatch :: Pattern -> Boolean
  alwaysMatch = case _ of
    PatVar _ -> true
    PatWildcard -> true
    PatNamed _ pat -> alwaysMatch pat
    _ -> false

type PatternMatrix =
  { patList :: Array Pattern
  , action :: ELambda
  }

data PatternMatching = PatternMatching
  -- `case head`
  (Array ELambda)
  -- list of patterns and corresponding action
  (Array PatternMatrix)

derive instance Generic PatternMatching _
instance Show PatternMatching where
  show = genericShow

sameToplevelSymbol :: Pattern -> Pattern -> Boolean
sameToplevelSymbol = case _, _ of
  PatWildcard, PatWildcard -> true
  PatVar _, PatVar _ -> true
  PatConst c1, PatConst c2 -> c1 == c2
  PatConstruct _ name1 _, PatConstruct _ name2 _ -> name1 == name2
  _, _ -> false

groupBy :: forall a. (a -> a -> Boolean) -> Array a -> Array (Array a)
groupBy f = go []
  where
  go results xs = case Array.uncons xs of
    Nothing -> results
    Just { head } ->
      case Array.filter (f head) xs, Array.filter (not <<< f head) xs of
        [], notSatis -> Array.snoc results notSatis
        satis, [] -> Array.snoc results satis
        satis, notSatis -> go (Array.snoc results satis) notSatis

xrange :: Int -> Int -> Array Int
xrange m n = do
  x <- m .. n
  guard (x < n)
  pure x