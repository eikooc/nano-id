module Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (length)
import Data.String.Utils (charAt)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Random (randomInt)
import Nano (create)

main :: Effect Unit
main = do
  let 
    alphabet = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz-"
  log alphabet
  el <- create (Just alphabet) 21
  logShow el

