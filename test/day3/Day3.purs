module Test.Day3 where

import Partial.Unsafe

import Data.Either (fromRight)
import Data.Functor ((<$>))
import Day3 (day3Part1, day3Part2)
import Effect (Effect)
import Prelude (Unit, bind, discard)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay3Part1
  testDay3Part2

testDay3Part1 :: Effect Unit
testDay3Part1 = do
  number <- unsafePartial fromRight <$> day3Part1 "test/day3/input.txt"
  assertEqual { actual: number, expected: 112378 }

testDay3Part2 :: Effect Unit
testDay3Part2 = do
  id <- unsafePartial fromRight <$> day3Part2 "test/day3/input.txt"
  assertEqual { actual: id, expected: 603 }
