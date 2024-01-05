module Purvasm.MiddleEnd.ELambda.Translate where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Reader (class MonadReader, asks)
import Data.Array as Array
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Exception.Unsafe (unsafeThrow)
import Partial.Unsafe (unsafeCrashWith)
import PureScript.CoreFn as CF
import Purvasm.MiddleEnd.ELambda.Syntax (ELambda(..))
import Purvasm.MiddleEnd.ELambda.Translate.Env (TranslEnv, searchLocalEnv)
import Purvasm.MiddleEnd.ELambda.Translate.Error (TranslError)
import Purvasm.MiddleEnd.Primitive (Primitive(..))
import Purvasm.MiddleEnd.Types (Ident, Occurrence(..), Var(..))

translAccess :: forall m. MonadError TranslError m => MonadReader TranslEnv m => Ident -> m (ELambda)
translAccess id = do
  var /\ (Occurrence path) <- searchLocalEnv id
  pure $
    foldr (\n lambda -> ELPrim (PGetField n) [ lambda ]) (ELVar var) path

translExpr :: forall m a. MonadError TranslError m => MonadReader TranslEnv m => CF.Expr a -> m ELambda
translExpr = case _ of 
  CF.ExprLet a binds e -> case Array.span (not <<< CF.isRec) binds of 
    { init, rest }
      | Array.null rest -> {-- Non-recursive let-in --}
      | Array.null init 
      | otherwise -> translExpr (CF.ExprLet a [head] (CF.ExprLet a tail e))
  _ -> pure $ ELVar (Var 0)

chunkLet :: forall a. CF.Expr a -> CF.Expr a
chunkLet expr = case expr of
  CF.ExprLet a bindings e ->
    case Array.uncons bindings of
      Nothing -> unsafeThrow "chunkLet: Empty binding group"
      Just { head, tail }
        | Array.null tail -> CF.ExprLet a [ head ] e
        | otherwise -> CF.ExprLet a [ head ] (chunkLet (CF.ExprLet a tail e))
  _ -> expr
