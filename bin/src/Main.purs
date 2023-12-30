module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Node.Cbor as Cbor
import Node.FS.Aff (readFile)
import Node.Path as Path
import PureScript.ExternsFile (ExternsFile)
import PureScript.ExternsFile.Decoder.Class (decoder)
import PureScript.ExternsFile.Decoder.Monad (runDecoder)

main :: Effect Unit
main = launchAff_ do
  path <- liftEffect $ Path.resolve [] "output/PureScript.ExternsFile/externs.cbor"
  buf <- readFile path
  res <- Cbor.decodeFirst buf <#> runDecoder (decoder@ExternsFile)
  logShow res