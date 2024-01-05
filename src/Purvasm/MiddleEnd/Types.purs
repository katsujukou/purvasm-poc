module Purvasm.MiddleEnd.Types
  ( Arity
  , AtomicConstant(..)
  , ConstructorTag(..)
  , Ident(..)
  , Occurrence(..)
  , StructureConstant(..)
  , Var(..)
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

data AtomicConstant
  = ACInt Int
  | ACNumber Number
  | ACBoolean Boolean
  | ACString String

derive instance Eq AtomicConstant
derive instance Ord AtomicConstant
derive instance Generic AtomicConstant _

instance Show AtomicConstant where
  show = genericShow

data ConstructorTag = TClosure

derive instance Eq ConstructorTag
derive instance Ord ConstructorTag
derive instance Generic ConstructorTag _

instance Show ConstructorTag where
  show = genericShow

data StructureConstant
  = SCAtom AtomicConstant
  | SCBlock ConstructorTag (Array StructureConstant)

derive instance Eq StructureConstant
derive instance Ord StructureConstant
derive instance Generic StructureConstant _

instance Show StructureConstant where
  show sc = genericShow sc

-- | A local variable representation.
-- | `newtype`ed integer value is so-called *de Bruijn index*.
newtype Var = Var Int

derive instance Eq Var
derive instance Ord Var
derive instance Newtype Var _

instance Show Var where
  show (Var v) = "(Var " <> show v <> ")"

type Arity = Int

newtype Occurrence = Occurrence (List Int)

derive instance Newtype Occurrence _
derive newtype instance Eq Occurrence
derive newtype instance Ord Occurrence
instance Show Occurrence where
  show (Occurrence o) = "(Occurrence " <> show o <> ")"

newtype Ident = Ident String

derive instance Newtype Ident _
derive instance Eq Ident
derive instance Ord Ident

instance Show Ident where
  show (Ident id) = "(Ident " <> id <> ")"