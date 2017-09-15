module Test.Main where

import Data.Record.Format
import Type.Data.Symbol
import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log $ format @"Hi {name}! Your favourite number is {number}" {name : "Bill", number : 16}
