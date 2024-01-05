module Purvasm.MiddleEnd.ELambda.Translate.Error where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Purvasm.MiddleEnd.Types (Ident)

data TranslError = UnknownLocal Ident

derive instance Generic TranslError _
instance Show TranslError where
  show = genericShow
