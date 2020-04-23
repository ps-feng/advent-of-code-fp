module Test.Day7 where

import Day7 (day7Part1, day7Part2)
import Effect (Effect)
import Prelude
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  -- testDay7Part1a
  --testDay7Part1b
  --testDay7Part2a
  testDay7Part2b

testDay7Part1a :: Effect Unit
testDay7Part1a = do
  actual <- day7Part1 "test/day7/input-test.txt"
  assertEqual { actual: actual, expected: "CABDFE" }

testDay7Part1b :: Effect Unit
testDay7Part1b = do
  actual <- day7Part1 "test/day7/input.txt"
  assertEqual { actual: actual, expected: "EPWCFXKISTZVJHDGNABLQYMORU" }

-- testDay7Part2a :: Effect Unit
-- testDay7Part2a = do
--   actual <- day7Part2 "test/day7/input-test.txt" 2
--   assertEqual { actual: actual, expected: 15 }

testDay7Part2b :: Effect Unit
testDay7Part2b = do
  actual <- day7Part2 "test/day7/input.txt" 5
  assertEqual { actual: actual, expected: 382 }
