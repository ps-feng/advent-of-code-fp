module Day9 (day9Part1 , day9Part2) where

import Prelude

import Data.Array ((..))
import Data.Foldable (class Foldable, foldl)
import Data.Map (Map, alter, empty, values) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Day9.CircularList (FocusedArray(..), current, insert, moveFocus, remove)
import Effect (Effect)

type Scores = M.Map Int Int

day9Part1 :: Int -> Int -> Effect Int
day9Part1 numPlayers numMarbles = do
  pure $ solve numPlayers (1 .. numMarbles)

day9Part2 :: Int -> Int -> Effect Int
day9Part2 numPlayers numMarbles = do
  pure $ solve numPlayers $ 1 .. (numMarbles * 100)

type State =
  { player :: Int
  , circle :: FocusedArray Int
  , scores :: Scores
  }

solve :: forall f. Foldable f 
      => Int    -- Number of playes
      -> f Int  -- Marble values
      -> Int    -- High score
solve numPlayers marbles =
  let
    initialState =
      { player: 0
      , circle: FocusedArray { currentIndex: 0, array: [0] }
      , scores: M.empty :: Scores}
  in
    maxScore $ foldl go initialState marbles
    where
      go :: State -> Int -> State
      go { player, circle, scores } marble =
        if (marble `mod` 23) /= 0 then
          let
            newCircle = insert marble $ moveFocus 1 circle
            -- bla = trace (show newCircle) \_ -> 1
          in
            { player: nextPlayer, circle: newCircle, scores }
        else
          let
            circleFocusedAtMinus7 = moveFocus (-7) circle
            marbleAtMinus7 = fromMaybe 0 $ current circleFocusedAtMinus7
            newCircle = remove circleFocusedAtMinus7
            -- bla = trace (show newCircle) \_ -> 1
            newScores = M.alter (\v -> Just $ (fromMaybe 0 v) + marbleAtMinus7 + marble) player scores
          in
            { player: nextPlayer, circle: newCircle, scores: newScores }
        where
          nextPlayer = (player + 1) `mod` numPlayers
  
      maxScore :: State -> Int
      maxScore { player: _, circle: _, scores } =
        foldl max 0 (M.values scores)

