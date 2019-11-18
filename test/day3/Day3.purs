module Test.Day3 where

import Day3 (day3Part1)
import Effect (Effect)
import Prelude (Unit, bind)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay3Part1

testDay3Part1 :: Effect Unit
testDay3Part1 = do
  number <- day3Part1
  assertEqual { actual: number, expected: 8892 }
