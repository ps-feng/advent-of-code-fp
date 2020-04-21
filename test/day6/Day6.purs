module Test.Day6 where
import Data.Maybe (fromMaybe)
import Day6 (day6Part1)
import Day6Brute (day6Part2)
import Effect (Effect)
import Prelude
import Test.Assert (assertEqual)
import Effect.Console as Console

main :: Effect Unit
main = do
  -- testDay6Part1a
  testDay6Part1b
  -- testDay6Part2a
  -- testDay6Part2b

testDay6Part1a :: Effect Unit
testDay6Part1a = do
  number <- day6Part1 "test/day6/input-test.txt"
  assertEqual { actual: fromMaybe 0 number, expected: 17 }

testDay6Part1b :: Effect Unit
testDay6Part1b = do
  number <- day6Part1 "test/day6/input.txt"
  assertEqual { actual: fromMaybe 0 number, expected: 3006 }

testDay6Part2a :: Effect Unit
testDay6Part2a = do
  number <- day6Part2 32 "test/day6/input-test.txt"
  assertEqual { actual: fromMaybe 0 number, expected: 16 }

testDay6Part2b :: Effect Unit
testDay6Part2b = do
  number <- day6Part2 10000 "test/day6/input.txt"
  assertEqual { actual: fromMaybe 0 number, expected: 42998 }
