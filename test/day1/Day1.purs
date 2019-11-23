module Test.Day1 where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Test.Assert (assertEqual)

import Day1 (day1Part1, day1Part2)

main :: Effect Unit
main = do
  testDay1Part1
  testDay1Part2

testDay1Part1 :: Effect Unit
testDay1Part1 = do
  result <- day1Part1
  assertEqual { actual: result, expected: Right 472.0 }

testDay1Part2 :: Effect Unit
testDay1Part2 = do
  result <- day1Part2
  assertEqual { actual: result, expected: Right 66932.0 }
