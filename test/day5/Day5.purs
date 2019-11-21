module Test.Day5 where

import Prelude
import Data.Either (Either(..))
import Day5 (day5Part1)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay5Part1

testDay5Part1 :: Effect Unit
testDay5Part1 = do
  str <- day5Part1 "test/day5/input-test.txt"
  assertEqual { actual: str, expected: "dabCBAcaDA" }

-- testDay5Part2 :: Effect Unit
-- testDay5Part2 = do
--   number <- day4Part2 "test/day5/input.txt"
--   assertEqual { actual: (Right 22687), expected: number }
