module Purvasm.MiddleEnd.Types
  ( (<:)
  , AccessPosition(..)
  , Arity
  , AtomicConstant(..)
  , ConstructorTag(..)
  , GlobalName(..)
  , Ident(..)
  , ModuleName(..)
  , Occurrence(..)
  , Primitive(..)
  , RecordShape
  , StructureConstant(..)
  , Var(..)
  , cons
  , mkGlobalName
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.List (List, (:))
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

type RecordShape = List String

data ConstructorTag
  = TArray
  | TRecord RecordShape
  | TClosure
  | TConstr Int

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
  = PGetGlobal GlobalName
  | PSetGlobal GlobalName
  | PGetField Int
  | PSetField Int
  | PMakeBlock ConstructorTag
  | PForeignCall String
  | PGetRecordField String
  | PGetBlockSize

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

data AccessPosition
  = APIndex Int
  | APKey String

derive instance Eq AccessPosition
derive instance Ord AccessPosition
derive instance Generic AccessPosition _
instance Show AccessPosition where
  show = genericShow

newtype Occurrence = Occurrence (List AccessPosition)

derive instance Newtype Occurrence _
derive newtype instance Eq Occurrence
derive newtype instance Ord Occurrence
derive newtype instance Semigroup Occurrence
derive newtype instance Monoid Occurrence

instance Show Occurrence where
  show (Occurrence o) = "(Occurrence " <> show o <> ")"

cons :: AccessPosition -> Occurrence -> Occurrence
cons i (Occurrence o) = Occurrence (i : o)

infixr 5 cons as <:

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

newtype GlobalName = GlobalName
  { modname :: ModuleName
  , ident :: Ident
  }

derive instance Generic GlobalName _
derive instance Eq GlobalName
derive instance Ord GlobalName
instance Show GlobalName where
  show (GlobalName gn) = "(GlobalName " <> show gn <> ")"

mkGlobalName :: ModuleName -> Ident -> GlobalName
mkGlobalName modname ident = GlobalName { modname, ident }