module Purvasm.MiddleEnd.ELambda.Translate.Env where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (class MonadReader, asks)
import Data.Array (foldr)
import Data.Array as Array
import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple, fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import PureScript.ExternsFile (ExternsDeclaration(..), ExternsFile(..)) as Ext
import PureScript.ExternsFile.Names (ModuleName(..), ProperName(..)) as Ext
import PureScript.ExternsFile.Types (TypeKind(..), SourceType) as Ext
import Purvasm.MiddleEnd.ELambda.Translate.Error (TranslError(..))
import Purvasm.MiddleEnd.Types (Arity, GlobalName, Ident(..), ModuleName(..), Occurrence, Var(..), mkGlobalName)
import Record as Record
import Type.Proxy (Proxy(..))

type TranslEnv =
  { moduleName :: ModuleName
  , local :: LocalVarEnv
  , global :: GlobalEnv
  -- , context :: Maybe TranslContext
  }

data TranslContext = FunctionBody

derive instance Generic TranslContext _
instance Show TranslContext where
  show = genericShow

emptyModuleEnv :: ModuleName -> TranslEnv
emptyModuleEnv moduleName = { moduleName, local: TNullEnv, global: emptyGlobalEnv }

emptyEnv :: TranslEnv
emptyEnv = emptyModuleEnv (ModuleName "")

data LocalVarEnv
  = TNullEnv
  | TReserved LocalVarEnv
  | TEnv (Array (Tuple Ident Occurrence)) LocalVarEnv

derive instance Generic LocalVarEnv _
instance Show LocalVarEnv where
  show env = genericShow env

emptyLocalEnv :: LocalVarEnv
emptyLocalEnv = TNullEnv

searchLocalEnv :: forall m. MonadError TranslError m => MonadReader TranslEnv m => Ident -> m (Tuple Var Occurrence)
searchLocalEnv id = asks _.local >>= go 0 >>> maybe (throwError $ UnknownLocal id) pure
  where
  go :: Int -> LocalVarEnv -> Maybe (Tuple Var Occurrence)
  go i = case _ of
    TNullEnv -> Nothing
    TReserved env -> go (i + 1) env
    TEnv vars env -> case vars # Map.fromFoldable >>> Map.lookup id of
      Nothing -> go (i + 1) env
      Just o -> Just (Var i /\ o)

extendByIdent :: forall f. Foldable f => f Ident -> TranslEnv -> TranslEnv
extendByIdent ids env0 = foldl (\env id -> env { local = TEnv [ id /\ mempty ] env.local }) env0 ids

type ConstrEnv = Map GlobalName ConstructorDesc

newtype GlobalEnv = GlobalEnv
  { values :: Map GlobalName ValueDesc
  , constructors :: ConstrEnv
  }

derive instance Newtype GlobalEnv _
instance Show GlobalEnv where
  show (GlobalEnv ge) = "(GlobalEnv " <> show ge <> ")"

type ValueDesc = {}

-- | Constructor descriptor
type ConstructorDesc =
  { arity :: Arity
  , tag :: Int
  }

emptyGlobalEnv :: GlobalEnv
emptyGlobalEnv = GlobalEnv
  { values: Map.empty
  , constructors: Map.empty
  }

externsEnv :: GlobalEnv -> Ext.ExternsFile -> GlobalEnv
externsEnv (GlobalEnv ge) (Ext.ExternsFile _ (Ext.ModuleName modname) _ _ _ _ decls _) = ge
  # flip (foldr applyDecl) decls
  # GlobalEnv
  where
  applyDecl :: Ext.ExternsDeclaration -> _ -> _
  applyDecl = case _ of
    Ext.EDType (Ext.ProperName _) _ ty
      | Ext.DataType _ _ constrs <- ty ->
          let
            applyConstr :: Array (Ext.ProperName /\ Array Ext.SourceType) -> ConstrEnv -> ConstrEnv
            applyConstr = flip $
              foldlWithIndex \tag constrEnv constrSig ->
                let
                  Ext.ProperName constr = fst constrSig
                  args = snd constrSig
                  ctorName = mkGlobalName (ModuleName modname) (Ident constr)
                in
                  Map.insert ctorName { arity: Array.length args, tag } constrEnv
          in
            Record.modify (Proxy @"constructors") (applyConstr constrs)
    _ -> identity