module Purvasm.MiddleEnd.ELambda.Error
  ( TranslError(..)
  , throwNotImplemented
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prim.TypeError (class Warn)
import Prim.TypeError as E
import Purvasm.MiddleEnd.Types (GlobalName, Ident)

data TranslError
  = UnknownLocal Ident
  | UnknownGlobal GlobalName
  | UnexpectedEmptyCaseHead
  | NotImplemented String

derive instance Generic TranslError _
instance Show TranslError where
  show = genericShow

throwNotImplemented
  :: forall m a
   . Warn (E.Text "Use of not implemented")
  => MonadError TranslError m
  => String
  -> m a
throwNotImplemented = throwError <<< NotImplemented
