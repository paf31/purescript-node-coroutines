module Test.Main where

import Prelude

import Control.Coroutine (connect, runProcess, transform, transformProducer)
import Control.Monad.Aff (runAff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Rec.Class (forever)
import Data.String (toUpper)
import Node.Coroutines (readable, writable)
import Node.Encoding (Encoding(..))
import Node.Process (stdin, stdout)

main :: Eff ( avar :: AVAR
            , console :: CONSOLE
            , exception :: EXCEPTION
            ) Unit
main = void <<< runAff logShow logShow <<< runProcess $
  readable UTF8 stdin
    `transformProducer` forever (transform toUpper)
    `connect` writable UTF8 stdout
