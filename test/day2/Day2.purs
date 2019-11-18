module Test.Day2 where

import Day2 (day2Part1, day2Part2)
import Effect (Effect)
import Prelude (Unit, bind)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay2Part1

testDay2Part1 :: Effect Unit
testDay2Part1 = do
  number <- day2Part1
  assertEqual { actual: number, expected: 8892 }

testDay2Part2 :: Effect Unit
testDay2Part2 = do
  id <- day2Part2
  assertEqual { actual: id, expected: "zihwtxagifpbsnwleydukjmqv"}
