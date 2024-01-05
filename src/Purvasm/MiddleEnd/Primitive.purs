-- | Types of primitive operation supported on the PurVASM.
module Purvasm.MiddleEnd.Primitive where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data Primitive
  = PGetGlobal String String
  | PSetGlobal String String
  | PGetField Int
  | PSetField Int
  | PMakeBlock
  | PForeignCall String

derive instance Eq Primitive
derive instance Ord Primitive
derive instance Generic Primitive _
instance Show Primitive where
  show = genericShow