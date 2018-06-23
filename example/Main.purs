-- Run this example with
-- 
--   pulp run -I example/
--
-- It will read lines incrementally from stdin, and output them in upper case
-- to stdout.
module Main where

import Prelude

import Effect.Aff (runAff)
import Effect (Effect)
import Effect.Console (logShow)
import Control.Monad.Rec.Class (forever)
import Control.Coroutine (connect, runProcess, transform, transformProducer)
import Data.Either (either)
import Data.String (toUpper)
import Node.Coroutines (readable, writable)
import Node.Encoding (Encoding(..))
import Node.Process (stdin, stdout)

main :: Effect Unit
main = void <<< runAff (either logShow logShow) <<< runProcess $
  readable UTF8 stdin
    `transformProducer` forever (transform toUpper)
    `connect` writable UTF8 stdout
