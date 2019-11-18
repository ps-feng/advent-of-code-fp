module Test.Day1 where

import Day1 (day1Part1, day1Part2)
import Effect (Effect)
import Prelude (Unit, bind, discard)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay1Part1
  testDay1Part2

testDay1Part1 :: Effect Unit
testDay1Part1 = do
  number <- day1Part1
  assertEqual { actual: number, expected: 472.0 }

testDay1Part2 :: Effect Unit
testDay1Part2 = do
  number <- day1Part2
  assertEqual { actual: number, expected: 66932.0 }
