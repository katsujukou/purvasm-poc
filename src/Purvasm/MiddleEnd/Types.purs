module Purvasm.MiddleEnd.Types
  ( Arity
  , AtomicConstant(..)
  , ConstructorTag(..)
  , Ident(..)
  , ModuleName(..)
  , Occurrence(..)
  , Primitive(..)
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
  | ACChar Char
  | ACString String

derive instance Eq AtomicConstant
derive instance Ord AtomicConstant
derive instance Generic AtomicConstant _

instance Show AtomicConstant where
  show = genericShow

data ConstructorTag
  = TArray
  | TRecord
  | TClosure

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

data Primitive
  = PGetGlobal ModuleName Ident
  | PSetGlobal ModuleName Ident
  | PGetField Int
  | PSetField Int
  | PMakeBlock ConstructorTag
  | PForeignCall String

derive instance Eq Primitive
derive instance Ord Primitive
derive instance Generic Primitive _
instance Show Primitive where
  show = genericShow

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
derive newtype instance Semigroup Occurrence
derive newtype instance Monoid Occurrence

instance Show Occurrence where
  show (Occurrence o) = "(Occurrence " <> show o <> ")"

newtype Ident = Ident String

derive instance Newtype Ident _
derive instance Eq Ident
derive instance Ord Ident

instance Show Ident where
  show (Ident id) = "(Ident " <> id <> ")"

newtype ModuleName = ModuleName String

derive instance Newtype ModuleName _
derive instance Eq ModuleName
derive instance Ord ModuleName

instance Show ModuleName where
  show (ModuleName id) = "(ModuleName " <> id <> ")"