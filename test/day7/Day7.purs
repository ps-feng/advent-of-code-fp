module Test.Day7 where

import Day7 (day7Part1)
import Effect (Effect)
import Prelude
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay7Part1a
  testDay7Part1b

testDay7Part1a :: Effect Unit
testDay7Part1a = do
  actual <- day7Part1 "test/day7/input-test.txt"
  assertEqual { actual: actual, expected: "CABDFE" }

testDay7Part1b :: Effect Unit
testDay7Part1b = do
  actual <- day7Part1 "test/day7/input.txt"
  assertEqual { actual: actual, expected: "EPWCFXKISTZVJHDGNABLQYMORU" }