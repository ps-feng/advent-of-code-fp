module Day1
  ( day1Part1
  , day1Part2
  ) where

import Data.List (List(..), foldl, fromFoldable, (:))
import Prelude (bind, map, pure, (+), (<$>))
import Data.Maybe (Maybe(..))
import Data.Number (fromString, nan)
import Data.Set as S
import Data.Traversable (sequence)
import Effect (Effect)
import Util

day1Part1 :: Effect Number
day1Part1 = do
  input <- readFileLines "test/day1/input.txt"
  let
    parsed :: Array (Maybe Number)
    parsed = fromString <$> input
  let
    floats :: Maybe (Array Number)
    floats = sequence parsed
  let
    result = foldl (+) 0.0 <$> floats
  case result of
    Just x -> pure x
    Nothing -> pure nan

day1Part2 :: Effect Number
day1Part2 = do
  input <- readFileLines "test/day1/input.txt"
  let
    parsed = fromString <$> input
  let
    floats = sequence parsed
  let
    maybeNumber = map day1Part2' floats
  case maybeNumber of
    Just x -> pure x
    Nothing -> pure nan

day1Part2' :: Array Number -> Number
day1Part2' [] = nan

day1Part2' deltas = part2' 0.0 (S.singleton 0.0) (fromFoldable deltas)
  where
  part2' :: Number -> S.Set Number -> List Number -> Number
  part2' accum seen Nil = part2' accum seen (fromFoldable deltas)

  part2' accum seen (x : xs) =
    let
      newAccum = accum + x
    in
      if newAccum `S.member` seen then
        newAccum
      else
        part2' newAccum (S.insert newAccum seen) xs

-- Lazy solution we could not get to work...
-- part2 :: Array Number -> Number
-- part2 deltas = part2' (S.singleton 0.0) (scanl (+) 0.0 $ cycle $ fromFoldable deltas)
--   where
--   part2' :: S.Set Number -> List Number -> Number 
--   part2' seen ds = case step ds of
--     Cons x xs ->
--       if x `S.member` seen then
--         trace (show x) (\_ -> x)
--       else
--         part2' (S.insert x seen) $ trace (show xs) (\_ -> xs)
--     _ -> nan
