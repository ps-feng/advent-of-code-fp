module Test.Day9 where

import Prelude

import Day9 (day9Part1)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay9Part1a

testDay9Part1a :: Effect Unit
testDay9Part1a = do
  actual <- day9Part1 416 71617
  assertEqual { actual: actual, expected: 436720 }
