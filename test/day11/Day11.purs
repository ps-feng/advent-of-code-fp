module Test.Day11 where

import Prelude

import Data.Maybe (Maybe(..))
import Day11 (SerialNumber, day11Part1)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testSolvePart1 18 29
  testSolvePart1 42 30

testSolvePart1 :: SerialNumber -> Int -> Effect Unit
testSolvePart1 serialNumber expectedPower = do
  actual <- day11Part1 serialNumber
  assertEqual { actual: actual, expected: Just expectedPower }
