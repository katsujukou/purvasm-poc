module Main where

import Prelude

import Data.Argonaut (parseJson)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.List (List(..))
import Data.Traversable (for)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Class.Console as Console
import Effect.Exception (throw)
import Effect.Ref as Ref
import Node.Cbor as Cbor
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readFile, readTextFile)
import Node.Path as Path
import PureScript.CoreFn as CF
import PureScript.CoreFn.Json as CFJ
import PureScript.ExternsFile (ExternsFile)
import PureScript.ExternsFile.Decoder.Class (decoder)
import PureScript.ExternsFile.Decoder.Monad (runDecoder)
import Purvasm.MiddleEnd.ELambda.Env (emptyModuleEnv, externsEnv)
import Purvasm.MiddleEnd.ELambda.Monad (runTranslM)
import Purvasm.MiddleEnd.ELambda.Translate (translExpr)
import Purvasm.MiddleEnd.Types (ModuleName(..))

main :: Effect Unit
main = launchAff_ do
  envRef <- liftEffect $ Ref.new (emptyModuleEnv (ModuleName "Sample"))
  path <- liftEffect $ Path.resolve [] "output"
  for_ [ "Sample" ] \md -> do
    buf <- readFile (Path.concat [ path, md, "externs.cbor" ])
    res <- Cbor.decodeFirst buf <#> runDecoder (decoder @ExternsFile)
    case res of
      Left e -> do
        liftEffect $ throw ("Failed to decode externs file: " <> md)
      Right externs -> liftEffect do
        Ref.modify_ (\env -> env { global = externsEnv env.global externs }) envRef
  env <- liftEffect $ Ref.read envRef
  Console.log "[env]"
  Console.logShow env
  json <- readTextFile UTF8 (Path.concat [ path, "Sample/corefn.json" ])
  let parsedModule = parseJson json >>= CFJ.decodeModule
  case parsedModule of
    Left _ -> liftEffect $ throw "failed to parse corefn module"
    Right m@(CF.Module { decls }) -> do
      logShow m
      let
        lambdaList = join <$> for decls case _ of
          CF.Rec decls' -> for decls' \(CF.Binding _ (CF.Ident ident) expr) -> do
            runTranslM env (translExpr expr)
          CF.NonRec (CF.Binding _ (CF.Ident ident) expr) -> do
            Array.singleton <$> runTranslM env (translExpr expr)

      case lambdaList of
        Right lambdas -> for_ lambdas \lambda -> do
          Console.logShow lambda -- (compileLambda "" lambda Nil)
        _ -> do
          Console.log "Oops!"

-- main :: Effect Unit
-- main = launchAff_ do
--   envRef <- liftEffect $ Ref.new (emptyModuleEnv (ModuleName "Data.Either"))
--   path <- liftEffect $ Path.resolve [] "output"
--   for_ [ "Sample", "Data.Either" ] \md -> do
--     buf <- readFile (Path.concat [ path, md, "externs.cbor" ])
--     res <- Cbor.decodeFirst buf <#> runDecoder (decoder @ExternsFile)
--     case res of
--       Left e -> do
--         liftEffect $ throw ("Failed to decode externs file: " <> md)
--       Right externs -> liftEffect do
--         Ref.modify_ (\env -> env { global = externsEnv env.global externs }) envRef
--   env <- liftEffect $ Ref.read envRef
--   Console.log "[env]"
--   Console.logShow env
--   json <- readTextFile UTF8 (Path.concat [ path, "Data.Either/corefn.json" ])
--   let parsedModule = parseJson json >>= CFJ.decodeModule
--   case parsedModule of
--     Left _ -> liftEffect $ throw "failed to parse corefn module"
--     Right (CF.Module { decls }) -> do
--       for_ decls case _ of
--         CF.Rec _ -> Console.log "Rec"
--         CF.NonRec (CF.Binding _ (CF.Ident ident) expr) -> do
--           Console.log $ "[" <> ident <> "]"
--           Console.logShow $ runTranslM env (translExpr expr)
