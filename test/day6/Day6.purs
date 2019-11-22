module Test.Day6 where

import Prelude
import Data.Maybe
import Day6
import Effect (Effect)
import Test.Assert (assertEqual)
import Effect.Console as Console
import Data.Array

main :: Effect Unit
main = do
  testa
  -- testDay6Part1a
  -- testDay6Part1b
  -- testDay6Part2

testa :: Effect Unit
testa = do
  Console.time "b"
  let sortedFast = sort $ positionsAtDistance (makeCoord 10 10) 10000
  Console.timeEnd "b"
  Console.time "a"
  let sortedBrute = sort $ bruteForceCoordinatesFromPositionAtDistance (makeCoord 10 10) 10000
  Console.timeEnd "a"
  assertEqual { actual: sortedFast, expected: sortedBrute }

testDay6Part1a :: Effect Unit
testDay6Part1a = do
  number <- day6Part1 "test/day6/input-test.txt"
  assertEqual { actual: fromMaybe 0 number, expected: 10 }

testDay6Part1b :: Effect Unit
testDay6Part1b = do
  number <- day6Part1 "test/day6/input.txt"
  assertEqual { actual: fromMaybe 0 number, expected: 9060 }

testDay6Part2 :: Effect Unit
testDay6Part2 = do
  
  number <- day6Part2 "test/day6/input.txt"
  assertEqual { actual: fromMaybe 0 number, expected: 6310 }
