module Test.Day8 where

import Day8 (day8Part1)
import Effect (Effect)
import Prelude
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay8Part1a
  testDay8Part1b

testDay8Part1a :: Effect Unit
testDay8Part1a = do
  actual <- day8Part1 "test/day8/input-test.txt"
  assertEqual { actual: actual, expected: 138 }

testDay8Part1b :: Effect Unit
testDay8Part1b = do
  actual <- day8Part1 "test/day8/input.txt"
  assertEqual { actual: actual, expected: 47464 }

-- -- testDay7Part2a :: Effect Unit
-- -- testDay7Part2a = do
-- --   actual <- day7Part2 "test/day7/input-test.txt" 2
-- --   assertEqual { actual: actual, expected: 15 }

-- testDay7Part2b :: Effect Unit
-- testDay7Part2b = do
--   actual <- day7Part2 "test/day7/input.txt" 5
--   assertEqual { actual: actual, expected: 382 }
