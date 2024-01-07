module Purvasm.MiddleEnd.ELambda.Translate.Env where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (class MonadReader, asks)
import Data.Foldable (class Foldable, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Purvasm.MiddleEnd.ELambda.Translate.Error (TranslError(..))
import Purvasm.MiddleEnd.Types (Ident, Occurrence(..), Var(..))

type TranslEnv =
  { local :: LocalVarEnv
  , global :: {}
  , context :: Maybe TranslContext
  }

data TranslContext = FunctionBody

emptyEnv :: TranslEnv
emptyEnv = { local: TNullEnv, global: {}, context: Nothing }

data LocalVarEnv
  = TNullEnv
  | TReserved LocalVarEnv
  | TEnv (Array (Tuple Ident Occurrence)) LocalVarEnv

searchLocalEnv :: forall m. MonadError TranslError m => MonadReader TranslEnv m => Ident -> m (Tuple Var Occurrence)
searchLocalEnv id = asks _.local >>= go 0 >>> maybe (throwError $ UnknownLocal id) pure
  where
  go :: Int -> LocalVarEnv -> Maybe (Tuple Var Occurrence)
  go i = case _ of
    TNullEnv -> Nothing
    TReserved env -> go (i + 1) env
    TEnv vars env -> case vars # Map.fromFoldable >>> Map.lookup id of
      Nothing -> go (i + 1) env
      Just o -> Just (Var i /\ o)

extendByIdent :: forall f. Foldable f => f Ident -> TranslEnv -> TranslEnv
extendByIdent ids env0 = foldl (\env id -> env { local = TEnv [ id /\ mempty ] env.local }) env0 ids