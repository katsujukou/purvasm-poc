-- Pattern matching compiler
module Purvasm.MiddleEnd.ELambda.PatternMatch where

import Prelude

import Control.Monad.Reader (class MonadReader, ask)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Generic.Rep (class Generic)
import Data.List (List(..), (:))
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested (type (/\), (/\))
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CoreFn as CF
import Purvasm.MiddleEnd.ELambda.Syntax (ELambda(..))
import Purvasm.MiddleEnd.ELambda.Translate.Env (GlobalEnv(..), TranslEnv, ConstructorDesc)
import Purvasm.MiddleEnd.Types (AtomicConstant(..), ConstructorTag(..), GlobalName, Ident(..), ModuleName(..), Primitive(..), StructureConstant(..), Arity, mkGlobalName)

type Expr = CF.Expr CF.Ann

type Binder = CF.Binder CF.Ann

data Pattern
  = PatWildcard
  | PatVar Ident
  | PatNamed Ident Pattern
  | PatConst StructureConstant
  | PatArray (Array Pattern)
  | PatRecord (Array (String /\ Pattern))
  | PatConstruct ConstructorDesc GlobalName (Array Pattern)

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

  constBinder :: Binder -> Maybe StructureConstant
  constBinder = case _ of
    CF.BinderLit _ lit -> case lit of
      CF.LitInt i -> Just (SCAtom (ACInt i))
      CF.LitNumber n -> Just (SCAtom (ACNumber n))
      CF.LitChar c -> Just (SCAtom (ACChar c))
      CF.LitString s -> Just (SCAtom (ACString s))
      CF.LitBoolean b -> Just (SCAtom (ACBoolean b))
      CF.LitArray xs -> SCBlock TArray <$> traverse constBinder xs
      CF.LitRecord xs ->
        SCBlock
          (TRecord $ L.foldl (\keys prop -> CF.propKey prop : keys) Nil xs)
          <$> traverse (constBinder <<< CF.propValue) xs
    _ -> Nothing

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

addToMatch :: PatternMatching -> PatternMatrix -> PatternMatching
addToMatch (PatternMatching head casel) cas = PatternMatching head (Array.cons cas casel)

-- In other words, this function extracts one line from matching,
-- discarding the left-most pattern and build single-line matching,
-- described as following:
-- 
--  e_1  ... e_k                
--  ↓        ↓                  
--  p_11 ... p_1k → act_1       e_2  ... e_k
--   :   ...  :   →  :     =>   ↓        ↓
--  p_m1 ... p_mk → act_m       p_j2 ...p_jk → act_j
-- 
makeConstantMatching :: Array ELambda -> PatternMatrix -> PatternMatching
makeConstantMatching heads mat = case Array.uncons heads of
  Nothing -> unsafeCrashWith "makeConstantMatching: emty case heads"
  Just { tail } -> PatternMatching tail [ mat ]

-- Building a single-line pattern matching matrix.
--
--  e_1  ... e_k                
--  ↓        ↓
--  p_11 ... p_1k → act_1
--   :   ...  :   →  :         e_1;[0]  e_1;[1] ... e_1;[n] e_2  ... e_k
--  p_j1 ... p_jk → act_j  =>   ↓        ↓      ...  ↓       ↓        ↓
--   :   ...  :   → :          q_1      q_2     ... q_n     p_j2 ... p_jk → act_j
--  p_m1 ... p_mk → act_k
--  where
--    p_j1 = C q_1 ... q_n
makeConstructorMatching :: Arity -> Array ELambda -> PatternMatrix -> PatternMatching
makeConstructorMatching arity heads mat = case Array.uncons heads of
  Nothing -> unsafeCrashWith "makeConstructorMatching: empty case heads"
  Just { head, tail } ->
    let
      expandedHeads = ST.run do
        expandedHeadsRef <- STArray.new
        argPosRef <- STRef.new 0
        ST.while ((_ < arity) <$> STRef.read argPosRef) do
          pos <- STRef.read argPosRef
          STArray.push (ELPrim (PGetField pos) [ head ]) expandedHeadsRef
          *> STRef.modify (_ + 1) argPosRef
          $> unit
        STArray.freeze expandedHeadsRef
    in
      PatternMatching (expandedHeads <> tail) [ mat ]

alwaysMatch :: Pattern -> Boolean
alwaysMatch = case _ of
  PatWildcard -> true
  PatVar _ -> true
  PatNamed _ pat -> alwaysMatch pat
  PatArray pats -> Array.all alwaysMatch pats
  PatRecord props -> Array.all (alwaysMatch <<< snd) props
  _ -> false

emptyMatching :: PatternMatching
emptyMatching = PatternMatching [] []

-- |
-- | Ω   p12 ... p1k -> a1
-- | Ω   p22 ... p2k -> a2
-- | ︙   ︙  ... ︙     ︙
-- | Ω   pi2 ... pik -> ai
-- | pj1 pj2 ... pjk -> b1
-- | ︙   ︙  ... ︙     ︙
-- | pm1 pm2 ... pmk -> bl 
splitMatching :: PatternMatching -> PatternMatching /\ PatternMatching
splitMatching (PatternMatching heads matrix) = case Array.uncons heads of
  Nothing -> unsafeCrashWith "splitMatching: Illegal empty case heads"
  Just { tail: casHeads } -> go (PatternMatching casHeads []) matrix
  where
  go vars mat = case Array.uncons mat of
    Nothing -> vars /\ (PatternMatching heads mat)
    Just { head: { patList, action }, tail: matRest } ->
      case Array.uncons patList of
        Nothing -> unsafeCrashWith "splitMatching: Illegal case matrix of zero rows"
        Just { head: pat, tail: patL }
          | alwaysMatch pat -> go (addToMatch vars { patList: patL, action }) matRest
          | otherwise -> vars /\ (PatternMatching heads mat)