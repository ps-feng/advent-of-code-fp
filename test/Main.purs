module Test.Main where

import Prelude
import Test.Day1 as Day1
import Test.Day2 as Day2
import Effect (Effect)

main :: Effect Unit
main = do
  Day1.main
  Day2.main
