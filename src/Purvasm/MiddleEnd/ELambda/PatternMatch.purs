-- | Pattern matching compiler
module Purvasm.MiddleEnd.ELambda.PatternMatch where

import Prelude

import Data.Array as Array
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import PureScript.CoreFn as CF
import Purvasm.MiddleEnd.Types (AccessPosition(..), Ident(..), Occurrence, (<:))

type Binder = CF.Binder CF.Ann

binderPaths :: Occurrence -> Binder -> Array (Tuple Ident Occurrence)
binderPaths occur = Array.fromFoldable <<< go occur
  where
  go o = case _ of
    CF.BinderVar _ (CF.Ident id) -> (Ident id /\ o) : Nil
    CF.BinderNamed _ (CF.Ident s) pat -> (Tuple (Ident s) occur) : go o pat
    CF.BinderConstructor _ _ _ patList -> binderListPaths 0 patList
    CF.BinderLit _ lit -> case lit of
      CF.LitArray patList -> binderListPaths 0 patList
      CF.LitRecord patPropList -> binderPropListPaths patPropList
      _ -> mempty
    _ -> mempty

  binderListPaths i = Array.uncons >>> case _ of
    Nothing -> mempty
    Just { head, tail } -> go (APIndex i <: occur) head <> binderListPaths (i + 1) tail

  binderPropListPaths = Array.uncons >>> case _ of
    Nothing -> mempty
    Just { head: CF.Prop prop pat, tail } -> go (APKey prop <: occur) pat <> binderPropListPaths tail