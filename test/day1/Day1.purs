module Test.Day1 where

import Prelude

import Day1 (day1)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  number <- day1
  assertEqual { actual: number, expected: 472.0 }
