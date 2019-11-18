module Test.Day1 where

import Prelude
import Effect (Effect)
import Test.Assert (assert)

main :: Effect Unit
main = do
  assert (1 == 2)
