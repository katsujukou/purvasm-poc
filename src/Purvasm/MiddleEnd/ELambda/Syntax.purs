module Purvasm.MiddleEnd.ELambda.Syntax where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Purvasm.MiddleEnd.Types (Primitive, StructureConstant, Var, Arity)

data ELambda
  = ELVar Var
  | ELConst StructureConstant
  | ELApply ELambda (Array ELambda)
  | ELFunction Arity ELambda
  | ELlet (Array ELambda) ELambda
  | ELletrec (Array ELambda) ELambda
  | ELPrim Primitive (Array ELambda)
  | ELIfThenElse ELambda ELambda ELambda
  | ELSequence ELambda ELambda
  | ELNone

derive instance Eq ELambda
derive instance Ord ELambda
derive instance Generic ELambda _

instance Show ELambda where
  show el = genericShow el
