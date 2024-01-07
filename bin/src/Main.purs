module Main where

import Prelude

import Data.Argonaut (parseJson)
import Data.Either (Either(..))
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Node.Cbor as Cbor
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, readTextFile)
import Node.Path as Path
import PureScript.CoreFn as CF
import PureScript.CoreFn.Json as CFJ
import PureScript.ExternsFile (ExternsFile)
import PureScript.ExternsFile.Decoder.Class (decoder)
import PureScript.ExternsFile.Decoder.Monad (runDecoder)
import Purvasm.MiddleEnd.ELambda.Translate (translExpr)
import Purvasm.MiddleEnd.ELambda.Translate.Env (emptyEnv)
import Purvasm.MiddleEnd.ELambda.Translate.Monad (runTranslM)

main :: Effect Unit
main = launchAff_ do
  path <- liftEffect $ Path.resolve [] "output/Sample/corefn.json"
  json <- readTextFile UTF8 path
  let parsedModule = parseJson json >>= CFJ.decodeModule
  case parsedModule of
    Left _ -> liftEffect $ throw "failed to parse module"
    Right (CF.Module { decls }) ->
      for_ decls case _ of
        CF.Rec _ -> Console.log "Rec"
        CF.NonRec (CF.Binding _ (CF.Ident ident) expr) -> do
          Console.log $ "[" <> ident <> "]"
          Console.logShow $ runTranslM emptyEnv (translExpr expr)
-- res <- Cbor.decodeFirst buf <#> runDecoder (decoder@ExternsFile)
-- logShow res