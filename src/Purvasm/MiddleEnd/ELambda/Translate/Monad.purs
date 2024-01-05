module Purvasm.MiddleEnd.ELambda.Translate.Monad where

import Prelude

import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ReaderT, runReaderT)
import Data.Either (Either)
import Data.Identity (Identity(..))
import Purvasm.MiddleEnd.ELambda.Translate.Env (TranslEnv)
import Purvasm.MiddleEnd.ELambda.Translate.Error (TranslError)

newtype TranslM a = TranslM (ReaderT TranslEnv (ExceptT TranslError Identity) a)

derive newtype instance Functor TranslM
derive newtype instance Apply TranslM
derive newtype instance Applicative TranslM
derive newtype instance Bind TranslM
derive newtype instance Monad TranslM
derive newtype instance MonadAsk TranslEnv TranslM
derive newtype instance MonadReader TranslEnv TranslM
derive newtype instance MonadThrow TranslError TranslM
derive newtype instance MonadError TranslError TranslM

runTranslM :: forall a. TranslEnv -> TranslM a -> Either TranslError a
runTranslM env (TranslM m) = let Identity a = runReaderT m env # runExceptT in a