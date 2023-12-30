module PureScript.ExternsFile where

import Prelude 


import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\))
import Fmt (fmtWith)
import PureScript.ExternsFile.Declarations (DeclarationRef)
import PureScript.ExternsFile.Decoder.Class (class Decode)
import PureScript.ExternsFile.Decoder.Generic (genericDecoder)
import PureScript.ExternsFile.Fmt (ShowRecordLikeConfig)
import PureScript.ExternsFile.Names (Ident, Qualified, ModuleName, ProperName, NameSource, OpName)
import PureScript.ExternsFile.SourcePos (SourceSpan)
import PureScript.ExternsFile.Types (ChainId, DataDeclType, FunctionalDependency,
                                     SourceConstraint, SourceType, TypeKind)


data ExternsFile = ExternsFile
  String
  ModuleName
  (Array DeclarationRef)
  (Array ExternsImport)
  (Array ExternsFixity)
  (Array ExternsTypeFixity)
  (Array ExternsDeclaration)
  SourceSpan

derive instance Generic ExternsFile _ 

instance Show ExternsFile where
  show = genericShow 

instance Decode ExternsFile where 
  decoder = genericDecoder

data ImportDeclarationType
  = Implicit
  | Explicit (Array DeclarationRef)
  | Hiding (Array DeclarationRef)

derive instance  Eq ImportDeclarationType
derive instance Ord ImportDeclarationType
derive instance Generic ImportDeclarationType _ 
instance Show ImportDeclarationType where
  show = genericShow

instance Decode ImportDeclarationType where
  decoder = genericDecoder

data ExternsImport = ExternsImport
  ModuleName
  ImportDeclarationType
  (Maybe ModuleName)

derive instance Eq ExternsImport
derive instance Ord ExternsImport
derive instance Generic ExternsImport _ 
instance Show ExternsImport where
  show = genericShow

instance Decode ExternsImport where
 decoder = genericDecoder 

type Precedence = Int

data Associativity
  = Infix
  | Infixl
  | Infixr

derive instance Eq Associativity
derive instance Ord Associativity
derive instance Generic Associativity _
instance Show Associativity where
  show = genericShow

instance Decode Associativity where
  decoder = genericDecoder 

data Fixity = Fixity Associativity Precedence

derive instance Eq Fixity
derive instance Ord Fixity
derive instance Generic Fixity _
instance Show Fixity where
  show = genericShow

data ExternsFixity = ExternsFixity
  Associativity
  Precedence
  OpName
  (Qualified (Either Ident ProperName))

derive instance Eq ExternsFixity
derive instance Ord ExternsFixity
derive instance Generic ExternsFixity _ 
instance Show ExternsFixity where
  show = genericShow

instance Decode ExternsFixity where
  decoder = genericDecoder 

data ExternsTypeFixity = ExternsTypeFixity Associativity Precedence OpName (Qualified ProperName)

derive instance Eq ExternsTypeFixity
derive instance Ord ExternsTypeFixity
derive instance Generic ExternsTypeFixity _ 
instance Show ExternsTypeFixity where
  show = genericShow

instance Decode ExternsTypeFixity where
  decoder = genericDecoder

data ExternsDeclaration
  = EDType
      ProperName    -- edTypeName
      SourceType    -- edTypeKind
      TypeKind      -- edTypeDeclarationKind
  | EDTypeSynonym 
      ProperName                            -- edTypeSynonymName
      (Array (String /\ Maybe SourceType))  -- edTypeSynonymArguments
      SourceType                            -- edTypeSynonymType
  | EDDataConstructor 
      ProperName        -- edDataCtorName
      DataDeclType      -- edDataCtorOrigin
      ProperName        -- edDataCtorTypeCtor
      SourceType        -- edDataCtorType
      (Array Ident)     -- edDataCtorFields
  | EDValue
      Ident             -- edValueName
      SourceType        -- edValueType
  | EDClass
      ProperName                           -- edClassName
      (Array (String /\ Maybe SourceType)) -- edClassTypeArguments
      (Array (Ident /\ SourceType))        -- edClassMembers
      (Array SourceConstraint)             -- edClassConstraints
      (Array FunctionalDependency)         -- edFunctionalDependencies
      Boolean                              -- edIsEmpty
  | EDInstance 
      (Qualified ProperName)               -- edInstanceClassName
      Ident                                -- edInstanceName
      (Array (Tuple String SourceType))    -- edInstanceForAll
      (Array SourceType)                   -- edInstanceKinds
      (Array SourceType)                   -- edInstanceTypes
      (Maybe (Array SourceConstraint))     -- edInstanceConstraints
      (Maybe ChainId)                      -- edInstanceChain
      Int                                  -- edInstanceChainIndex
      NameSource                           -- edInstanceNameSource
      SourceSpan                           -- edInstanceSourceSpan

instance Show ExternsDeclaration where 
  show = case _ of
    EDType pn ty ki -> fmtWith
      @ShowRecordLikeConfig
      @"EDType { name = <pn>, kind = <ty>, declarationKind = <ki> }"
      { pn: show pn, ty: show ty, ki: show ki }
    EDTypeSynonym pn args ty -> fmtWith 
      @ShowRecordLikeConfig
      @"EDTypeSynonym { name = <pn>, arguments = <args>, type = <ty> }"
      { pn: show pn, args: show args, ty: show ty }
    EDDataConstructor name orig typ sig flds -> fmtWith 
      @ShowRecordLikeConfig
      @"EDDataConstructor { name = <name>, origin = <orig>, type = <typ>, signature = <sig>, fields = <flds> }"
      { name: show name
      , orig: show orig
      , sig: show sig
      , typ: show typ
      , flds: show flds
      }
    EDValue name typ -> fmtWith
      @ShowRecordLikeConfig
      @"EDValue { name = <name>, type = <typ> }"
      { name: show name, typ: show typ }
    EDClass name typargs members constraints fundeps isEmpty -> fmtWith
      @ShowRecordLikeConfig
      @"EDClass { name = <name>, typeArgs = <typeArgs>, members = <members>, constraints = <constraints>, fundeps = <fundeps>, isEmpty = <isEmpty> }"
      { name: show name
      , typeArgs: show typargs
      , members: show members
      , constraints: show constraints
      , fundeps: show fundeps
      , isEmpty: show isEmpty
      }
    EDInstance clsName instName polyTyps kind typ consts ch chIdx nameSrc ss -> fmtWith
      @ShowRecordLikeConfig
      @"EDInstance { className = <clsName>, instanceName = <instName>, forall = <polyTypes>, kind = <kind>, type = <typ>, constraints = <consts>, chain = <ch>, chainIndex = <chIdx>, nameSource = <nameSrc>, sourceSpan = <ss> }"
      { clsName: show clsName
      , instName: show instName
      , polyTypes: show polyTyps
      , kind: show kind
      , typ: show typ
      , consts: show consts
      , ch: show ch
      , chIdx: show chIdx
      , nameSrc: show nameSrc
      , ss: show ss
      }   

derive instance Generic ExternsDeclaration _

instance Decode ExternsDeclaration where
  decoder = genericDecoder