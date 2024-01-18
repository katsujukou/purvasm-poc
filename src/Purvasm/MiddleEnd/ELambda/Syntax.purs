module Purvasm.MiddleEnd.ELambda.Syntax where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Purvasm.MiddleEnd.Types (Arity, AtomicConstant, Primitive, StructureConstant, Var)

data ELambda
  = ELVar Var
  | ELConst StructureConstant
  | ELApply ELambda (Array ELambda)
  | ELFunction Arity ELambda
  | ELStaticFail
  | ELStaticHandle ELambda ELambda
  | ELConditional ELambda (Array (Tuple AtomicConstant ELambda))
  | ELswitch ELambda (Array (Tuple Int ELambda))
  | ELlet (Array ELambda) ELambda
  | ELletrec (Array ELambda) ELambda
  | ELPrim Primitive (Array ELambda)
  | ELifthenelse ELambda ELambda ELambda
  | ELSequence ELambda ELambda
  | ELNone

derive instance Eq ELambda
derive instance Ord ELambda
derive instance Generic ELambda _

instance Show ELambda where
  show el = genericShow el
