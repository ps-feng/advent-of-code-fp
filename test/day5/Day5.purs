module Test.Day5 where

import Prelude
import Data.Either (Either(..))
import Day5 (day5Part1)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay5Part1a
  testDay5Part1b

testDay5Part1a :: Effect Unit
testDay5Part1a = do
  number <- day5Part1 "test/day5/input-test.txt"
  assertEqual { actual: number, expected: 10 }

testDay5Part1b :: Effect Unit
testDay5Part1b = do
  number <- day5Part1 "test/day5/input.txt"
  assertEqual { actual: number, expected: 9060 }

-- testDay5Part2 :: Effect Unit
-- testDay5Part2 = do
--   number <- day5Part2 "test/day5/input.txt"
--   assertEqual { actual: (Right 22687), expected: number }
