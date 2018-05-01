module Test.Main where

import Prelude

import Data.Record.Format (format)
import Effect (Effect)
import Test.Assert (assert)
import Type.Prelude (SProxy(..))

main :: Effect Unit
main = do
  let formatted = format (SProxy :: SProxy "Hi {name}! You are {number}") {name : "Bill", number : 16}
  assert $ formatted == "Hi Bill! You are 16"
