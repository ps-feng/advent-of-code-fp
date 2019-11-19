module Test.Day4 where

import Partial.Unsafe
import Prelude

import Data.Either (Either(..))
import Data.Functor ((<$>))
import Day4 (day4Part1)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay4Part1

testDay4Part1 :: Effect Unit
testDay4Part1 = do
  number <- day4Part1 "test/day4/input-test.txt"
  assertEqual { actual: (Right ""), expected: show <$> number }
