module Test.Day9 where

import Prelude

import Data.Foldable (for_)
import Day9 (day9Part1)
import Effect (Effect)
import Test.Assert (assertEqual)

main :: Effect Unit
main = do
  testDay9Part1Examples
  testDay9Part1a

testDay9Part1Examples :: Effect Unit
testDay9Part1Examples =
  let examples = [ { numPlayers: 10, lastMarbleValue: 1618, expectedScore:   8317 }
                 , { numPlayers: 13, lastMarbleValue: 7999, expectedScore: 146373 }
                 , { numPlayers: 17, lastMarbleValue: 1104, expectedScore:   2764 }
                 , { numPlayers: 21, lastMarbleValue: 6111, expectedScore:  54718 }
                 , { numPlayers: 30, lastMarbleValue: 5807, expectedScore:  37305 }
                 ]
  in
    for_ examples \{ numPlayers, lastMarbleValue, expectedScore } -> 
      doTestDay9Part1 numPlayers lastMarbleValue expectedScore

doTestDay9Part1 :: Int -- Number of players
                -> Int -- Last marble value
                -> Int -- Expected highest score
                -> Effect Unit
doTestDay9Part1 numPlayers lastMarbleValue expectedScore = do
  actual <- day9Part1 numPlayers lastMarbleValue
  assertEqual { actual: actual, expected: expectedScore }

testDay9Part1a :: Effect Unit
testDay9Part1a = do
  actual <- day9Part1 416 71617
  assertEqual { actual: actual, expected: 436720 }
