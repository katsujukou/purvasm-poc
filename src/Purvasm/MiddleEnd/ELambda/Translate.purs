module Purvasm.MiddleEnd.ELambda.Translate where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Reader (class MonadReader, ask, asks, local)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array ((..))
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Foldable (foldr)
import Data.List as L
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CoreFn as CF
import Purvasm.MiddleEnd.ELambda.Syntax (ELambda(..))
import Purvasm.MiddleEnd.ELambda.Translate.Env (GlobalEnv(..), LocalVarEnv(..), TranslEnv, extendByIdent, searchLocalEnv)
import Purvasm.MiddleEnd.ELambda.Translate.Error (TranslError(..), throwNotImplemented)
import Purvasm.MiddleEnd.Types (AtomicConstant(..), ConstructorTag(..), Ident(..), ModuleName(..), Occurrence(..), Primitive(..), StructureConstant(..), Var(..), Arity, mkGlobalName)
import Record as Record
import Type.Proxy (Proxy(..))

type Expr = CF.Expr CF.Ann
type Bind = CF.Bind CF.Ann
type Binder = CF.Binder CF.Ann

translAccess :: forall m. MonadError TranslError m => MonadReader TranslEnv m => Ident -> m (ELambda)
translAccess id = do
  var /\ (Occurrence path) <- searchLocalEnv id
  pure $
    foldr (\n lambda -> ELPrim (PGetField n) [ lambda ]) (ELVar var) path

constantExpr :: Expr -> Maybe StructureConstant
constantExpr = case _ of
  CF.ExprLit _ lit -> constantLiteral lit
  _ -> Nothing
  where
  constantLiteral :: CF.Literal Expr -> Maybe StructureConstant
  constantLiteral = case _ of
    CF.LitInt i -> Just $ SCAtom $ ACInt i
    CF.LitBoolean b -> Just $ SCAtom $ ACBoolean b
    CF.LitChar c -> Just $ SCAtom $ ACChar c
    CF.LitNumber n -> Just $ SCAtom $ ACNumber n
    CF.LitString s -> Just $ SCAtom $ ACString s
    CF.LitArray elems -> traverse constantExpr elems <#> SCBlock TArray
    CF.LitRecord _ -> Nothing

translExpr :: forall m. MonadError TranslError m => MonadReader TranslEnv m => Expr -> m ELambda
translExpr = case _ of
  CF.ExprVar _ var -> case var of
    CF.Qualified Nothing id -> translAccess (translIdent id)
    CF.Qualified (Just mn) id -> do
      { global: GlobalEnv ge } <- ask
      let globalName = mkGlobalName (translModuleName mn) (translIdent id)
      case Map.lookup globalName ge.constructors of
        Just desc
          | desc.arity == 0 -> pure $ ELConst (SCBlock (TConstr desc.tag) [])
        _ -> pure $ ELPrim (PGetGlobal $ mkGlobalName (translModuleName mn) (translIdent id)) []
  elit@(CF.ExprLit _ lit) -> case constantExpr elit of
    Just sc -> pure (ELConst sc)
    Nothing -> case lit of
      CF.LitArray elems -> ELPrim (PMakeBlock TArray) <$> traverse translExpr elems
      CF.LitRecord _ -> throwNotImplemented "Record literal"
      _ -> unsafeCrashWith "translExpr:ExprLit non-constant literal"
  f@(CF.ExprAbs _ _ _) -> translAbs 0 f
  CF.ExprApp _ abs arg -> case uncurryNaryApp abs arg of
    func /\ args
      | CF.ExprVar _ (CF.Qualified (Just mn) id) <- func -> do
          translAppWithGlobalName (translModuleName mn) (translIdent id) args
      | otherwise ->
          ELApply <$> (translExpr func) <*> (traverse translExpr args)
  CF.ExprLet a binds e -> translExprLet a binds e
  CF.ExprConstructor _ _ ctor _ -> translExprConstructor ctor
  e -> throwNotImplemented (exprType e)

  where
  translAbs :: Arity -> Expr -> m ELambda
  translAbs arity = case _ of
    CF.ExprAbs _ id e -> local (extendByIdent [ translIdent id ]) (translAbs (arity + 1) e)
    e -> ELFunction arity <$> translExpr e

  uncurryNaryApp :: _ -> _ -> Expr /\ Array Expr
  uncurryNaryApp f arg0 = ST.run do
    refArgs <- STArray.thaw [ arg0 ]
    refAbs <- STRef.new f
    continue <- STRef.new true
    ST.while (STRef.read continue) do
      STRef.read refAbs >>= case _ of
        CF.ExprApp _ abs arg -> do
          STRef.write abs refAbs
            *> STArray.unshift arg refArgs
            $> unit
        _ -> STRef.write false continue $> unit
    (/\) <$> STRef.read refAbs <*> STArray.freeze refArgs

  translAppWithGlobalName :: ModuleName -> Ident -> Array Expr -> m ELambda
  translAppWithGlobalName mn id args = do
    { global: GlobalEnv ge } <- ask
    trArgs <- traverse translExpr args
    let globalName = mkGlobalName mn id
    pure $ case Map.lookup globalName ge.constructors of
      Just desc
        -- Constructor fully applied 
        | desc.arity == Array.length args ->
            case traverse elambdaConst trArgs of
              Just constArgs -> ELConst (SCBlock (TConstr desc.tag) constArgs)
              _ -> ELPrim (PMakeBlock (TConstr desc.tag)) trArgs
      -- Constructor partially applied or Unknown constroctor (or maybe should we trap this case ?)
      _ -> ELApply (ELPrim (PGetGlobal globalName) []) trArgs

  translExprLet :: CF.Ann -> Array Bind -> Expr -> m ELambda
  translExprLet a binds e = case spanMap CF.nonRecBinding binds of
    [] /\ rest
      | Just { head: CF.Rec bindGrp, tail } <- Array.uncons rest ->
          let
            body = CF.ExprLet a tail e
            ids = translIdent <<< CF.bindingIdent <$> bindGrp
            exprs = CF.bindingExpr <$> bindGrp
            extendEnv = extendByIdent ids
          in
            ELletrec
              <$> (local extendEnv (traverse translExpr exprs))
              <*> local extendEnv (translExpr body)
    nonRecs /\ rest ->
      let
        body = if (Array.null rest) then e else CF.ExprLet a rest e
        ids = (translIdent <<< CF.bindingIdent) <$> nonRecs
        exprs = CF.bindingExpr <$> nonRecs
      in
        ELlet <$> translLet exprs <*> local (extendByIdent ids) (translExpr body)

  translExprConstructor ctor = do
    { moduleName: mn, global: GlobalEnv { constructors: constrEnv } } <- ask
    let globalConstrName = mkGlobalName mn (translIdent ctor)
    case Map.lookup globalConstrName constrEnv of
      Nothing -> throwError $ UnknownGlobal globalConstrName
      Just desc
        | desc.arity > 0 ->
            let
              prim = ELPrim (PMakeBlock (TConstr desc.tag))
              args = map (ELVar <<< Var) $ Array.reverse (0 .. (desc.arity - 1))
              body = prim args
            in
              pure $ ELFunction desc.arity body
        | otherwise -> pure ELNone

  translIdent :: CF.Ident -> Ident
  translIdent (CF.Ident id) = Ident id

  translModuleName :: CF.ModuleName -> ModuleName
  translModuleName (CF.ModuleName mn) = ModuleName mn

  translLet :: Array Expr -> m (Array ELambda)
  translLet = map Array.fromFoldable <<< go
    where
    go = Array.uncons >>> case _ of
      Nothing -> pure L.Nil
      Just { head, tail } ->
        L.Cons
          <$> translExpr head
          <*> local (Record.modify (Proxy @"local") TReserved) (go tail)

  exprType = case _ of
    CF.ExprVar _ _ -> "ExprVar"
    CF.ExprLit _ _ -> "ExprLit"
    CF.ExprAbs _ _ _ -> "ExprAbs"
    CF.ExprApp _ _ _ -> "ExprApp"
    CF.ExprAccessor _ _ _ -> "ExprAccessor"
    CF.ExprUpdate _ _ _ -> "ExprUpdate"
    CF.ExprLet _ _ _ -> "ExprLet"
    CF.ExprConstructor _ _ _ _ -> "ExprConstructor"
    CF.ExprCase _ _ _ -> "ExprCase"

elambdaConst :: ELambda -> Maybe StructureConstant
elambdaConst (ELConst sc) = Just sc
elambdaConst _ = Nothing

translPatternMatch
  :: forall m
   . MonadError TranslError m
  => MonadReader TranslEnv m
  => Array (Array Binder /\ Expr)
  -> m ELambda
translPatternMatch patternExprs = do
  pure $ ELVar (Var 0)

spanMap :: forall a b. (a -> Maybe b) -> Array a -> Array b /\ Array a
spanMap pred xs = ST.run do
  init <- STArray.new
  rest <- STArray.thaw xs
  continue <- STRef.new true
  ST.while (STRef.read continue) do
    STArray.shift rest >>= case _ of
      Nothing -> STRef.write false continue $> unit
      Just r
        | Just b <- pred r -> STArray.push b init $> unit
        | otherwise -> STArray.unshift r rest *> STRef.write false continue $> unit
  (/\) <$> STArray.freeze init <*> STArray.freeze rest