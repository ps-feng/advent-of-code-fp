module Day9 (day9Part1 , day9Part2) where

import Prelude

import Data.Array ((..))
import Data.BigInt (BigInt, fromInt)
import Data.Foldable (class Foldable, foldl)
import Data.Map (Map, alter, empty, values) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Day9.CircularList (current, insert, moveFocus, remove)
import Day9.ListZipper (ListZipper, singleton)
import Effect (Effect)

type Scores = M.Map Int BigInt

day9Part1 :: Int -> Int -> Effect BigInt
day9Part1 numPlayers numMarbles = do
  pure $ solve numPlayers (1 .. numMarbles)

day9Part2 :: Int -> Int -> Effect BigInt
day9Part2 numPlayers numMarbles = do
  pure $ solve numPlayers $ 1 .. (numMarbles * 100)

type State =
  { player :: Int
  , circle :: ListZipper Int
  , scores :: Scores
  }

solve :: forall f. Foldable f 
      => Int    -- Number of playes
      -> f Int  -- Marble values
      -> BigInt    -- High score
solve numPlayers marbles =
  let
    initialState =
      { player: 0
      , circle: singleton 0
      , scores: M.empty :: Scores
      }
  in
    maxScore $ foldl go initialState marbles
    where
      go :: State -> Int -> State
      go state@{ player, circle, scores } marble =
        if (marble `mod` 23) /= 0 then
          let
            newCircle = insert marble $ moveFocus 1 circle
          in
            { player: nextPlayer, circle: newCircle, scores }
        else
          let
            circleFocusedAtMinus7 = moveFocus (-7) circle
            marbleAtMinus7 = fromMaybe 0 $ current circleFocusedAtMinus7
            newCircle = remove circleFocusedAtMinus7
            newScores = M.alter (
              \v -> 
                Just $ (fromMaybe (fromInt 0) v) 
                     + fromInt(marbleAtMinus7 + marble)
            ) player scores
          in
            { player: nextPlayer, circle: newCircle, scores: newScores }
        where
          nextPlayer = (player + 1) `mod` numPlayers
  
      maxScore :: State -> BigInt
      maxScore { player: _, circle: _, scores } =
        foldl max (fromInt 0) (M.values scores)

