module Day2
  ( day2Part1
  , day2Part2
  ) where

import Data.Array (cons, filter, groupBy, length, sort, zip)
import Data.Array.NonEmpty as NonEmpty
import Data.List (List(..), elem, foldr, fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Prelude (bind, map, pure, (#), ($), (*), (-), (==), (>>>))
import Util (readFileLines)

day2Part1 :: Effect Int
day2Part1 = do
  input <- readFileLines "test/day2/input.txt"
  pure $ checksum input

checksum :: Array String -> Int
checksum lines =
  let
    occurrencies = map countOccurrencies lines

    twos = length $ filter (\xs -> 2 `elem` xs) occurrencies

    threes = length $ filter (\xs -> 3 `elem` xs) occurrencies
  in
    twos * threes

countOccurrencies :: String -> Array Int
countOccurrencies =
  toCharArray
    >>> sort
    >>> groupBy (\a b -> a == b)
    >>> map NonEmpty.length

day2Part2 :: Effect String
day2Part2 = do
  input <- readFileLines "test/day2/input.txt"
  let
    maybeBoxId = findBoxId (fromFoldable input)
  pure case maybeBoxId of
    Just id -> id
    Nothing -> ""

findBoxId :: List String -> Maybe String
findBoxId Nil = Nothing

findBoxId (x : xs) =
  let
    maybeId = findBoxId' x xs
  in
    case maybeId of
      Just id -> Just id
      Nothing -> findBoxId xs

findBoxId' :: String -> List String -> Maybe String
findBoxId' x Nil = Nothing

findBoxId' x (headXs : tailXs) =
  let
    maybeStr = stringWithOneDifferentChar x headXs
  in
    case maybeStr of
      Just str -> Just str
      Nothing -> findBoxId' x tailXs

stringWithOneDifferentChar :: String -> String -> Maybe String
stringWithOneDifferentChar s1 s2 =
  let
    result =
      zip (toCharArray s1) (toCharArray s2)
        # filter (\(Tuple l r) -> l == r)
        # foldr (\(Tuple a _) b -> cons a b) []
  in
    case String.length s1 - length result of
      1 -> Just $ fromCharArray result
      _ -> Nothing
