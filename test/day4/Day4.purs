module Test.Day4 where

import Prelude
import Data.Either (Either(..))
import Day4 (day4Part1, day4Part2)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay4Part1
  testDay4Part2

testDay4Part1 :: Effect Unit
testDay4Part1 = do
  number <- day4Part1 "test/day4/input.txt"
  assertEqual { actual: number, expected: (Right 19874) }

testDay4Part2 :: Effect Unit
testDay4Part2 = do
  number <- day4Part2 "test/day4/input.txt"
  assertEqual { actual: number, expected: (Right 22687) }
