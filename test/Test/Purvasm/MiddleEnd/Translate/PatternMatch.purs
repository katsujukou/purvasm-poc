module Test.Purvasm.MiddleEnd.Translate.PatternMatch where

import Prelude

import Control.Monad.Reader (runReaderT)
import PureScript.CoreFn as CF
import Purvasm.MiddleEnd.ELambda.PatternMatch (Pattern(..), binderToPattern)
import Purvasm.MiddleEnd.ELambda.Env (emptyEnv)
import Purvasm.MiddleEnd.Types (Ident(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec = describe "Purvasm.MiddleEnd.Translate.PatternMatch" do
  describe "Translate corefn `Binder` to corresponding `Pattern`" do
    it "should translate BinderNull to PatWildcard" do
      pat <- runReaderT (binderToPattern (CF.BinderNull CF.emptyAnn)) emptyEnv
      pat `shouldEqual` PatWildcard

    it "should translate BinderVar to PatVar" do
      pat <- runReaderT (binderToPattern (CF.BinderVar CF.emptyAnn (CF.Ident "x"))) emptyEnv
      pat `shouldEqual` (PatVar (Ident "x"))