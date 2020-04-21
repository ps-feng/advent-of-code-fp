module Test.Day6 where

import Data.Maybe (fromMaybe)
import Day6 (day6Part1)
import Effect (Effect)
import Prelude
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay6Part1a

testDay6Part1a :: Effect Unit
testDay6Part1a = do
  number <- day6Part1 "test/day6/input-test.txt"
  assertEqual { actual: fromMaybe 0 number, expected: 17 }

-- testDay6Part1b :: Effect Unit
-- testDay6Part1b = do
--   number <- day6Part1 "test/day6/input.txt"
--   assertEqual { actual: fromMaybe 0 number, expected: 10 }
