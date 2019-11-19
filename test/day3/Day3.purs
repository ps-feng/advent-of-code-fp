module Test.Day3 where

import Day3 (day3Part1)
import Data.Either (fromRight)
import Data.Functor ((<$>))
import Effect (Effect)
import Partial.Unsafe
import Prelude (Unit, bind)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay3Part1

testDay3Part1 :: Effect Unit
testDay3Part1 = do
  number <- unsafePartial fromRight <$> day3Part1 "test/day3/input.txt"
  assertEqual { actual: number, expected: 112378 }
