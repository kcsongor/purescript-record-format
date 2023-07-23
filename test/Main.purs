module Test.Main where

import Prelude

import Effect (Effect)
import Record.Format (format)
import Test.Assert (assert)

main :: Effect Unit
main = do
  let formatted = format @"Hi {name}! You are {number}" {name : "Bill", number : 16}
  assert $ formatted == "Hi Bill! You are 16"
