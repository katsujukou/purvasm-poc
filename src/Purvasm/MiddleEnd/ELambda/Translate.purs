module Purvasm.MiddleEnd.ELambda.Translate where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Reader (class MonadReader, ask, asks, local)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.ST as ST
import Control.Monad.ST.Internal as STRef
import Data.Array as Array
import Data.Array.ST as STArray
import Data.Foldable (foldr)
import Data.List as L
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested (type (/\), (/\))
import Debug (spy)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CoreFn as CF
import Purvasm.MiddleEnd.ELambda.Syntax (ELambda(..))
import Purvasm.MiddleEnd.ELambda.Translate.Env (LocalVarEnv(..), TranslEnv, extendByIdent, searchLocalEnv)
import Purvasm.MiddleEnd.ELambda.Translate.Error (TranslError, throwNotImplemented)
import Purvasm.MiddleEnd.Types (AtomicConstant(..), ConstructorTag(..), Ident(..), ModuleName(..), Occurrence(..), Primitive(..), StructureConstant(..), Var(..))
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
  elit@(CF.ExprLit _ lit) -> case constantExpr elit of
    Just sc -> pure (ELConst sc)
    Nothing -> case lit of
      CF.LitArray elems -> ELPrim (PMakeBlock TArray) <$> traverse translExpr elems
      CF.LitRecord _ -> throwNotImplemented "Record literal"
      _ -> unsafeCrashWith "translExpr:ExprLit non-constant literal"
  f@(CF.ExprAbs _ _ _) -> translAbs 0 f
  CF.ExprVar _ var -> case var of
    CF.Qualified Nothing id -> translAccess (translIdent id)
    CF.Qualified (Just mn) id -> pure $ ELPrim (PGetGlobal (translModuleName mn) (translIdent id)) []
  -- Note: This translation is not complete because
  -- We have to consider the case where `abs` is Constructor or global.
  -- In the first case, we need to check whether that constructor is fully applied and 
  -- and those arguments are all constant, where we should translate this application into
  -- the primitive operation `ELConst`, and if one of these arguments is not constant, then
  -- we translate this into `ELPrim (MakeBlock constructorTag)`.
  -- In the second case, we should translate this into prim GetGlobal.  
  CF.ExprApp _ abs arg ->
    let
      func /\ args = uncurryNaryApp abs arg
    in
      ELApply <$> (translExpr func) <*> (traverse translExpr args)
  CF.ExprLet a binds e -> translExprLet a binds e
  _ -> pure $ ELVar (Var 0)

  where
  translAbs arity = case _ of
    CF.ExprAbs _ id e -> local (extendByIdent [ translIdent id ]) (translAbs (arity + 1) e)
    e -> ELFunction arity <$> translExpr e

  -- uncurryAbs f arg0 = ST.run do
  --   refArgs <- STArray.thaw [ CF.BinderVar CF.emptyAnn arg0 ]
  --   refBody <- STRef.new f
  --   continue <- STRef.new true
  --   ST.while (STRef.read continue) do
  --     STRef.read refBody >>= case _ of
  --       CF.ExprAbs _ arg body -> do
  --         STRef.write body refBody
  --           *> STArray.push (CF.BinderVar CF.emptyAnn arg) refArgs
  --           $> unit
  --       _ -> STRef.write false continue $> unit
  --   (/\) <$> STArray.freeze refArgs <*> STRef.read refBody

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