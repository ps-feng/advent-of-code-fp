module Day3
  (day3Part1) where

import Control.Apply
import Control.Bind
import Data.Array.NonEmpty.Internal
import Data.Either
import Data.Functor
import Data.String.Regex
import Data.String.Regex.Flags

import Data.Array (cons, filter, groupBy, length, sort, zip)
import Data.Array.NonEmpty as NonEmpty
import Data.Int (fromString)
import Data.Int.Bits (xor)
import Data.List (List(..), elem, foldr, fromFoldable, (:))
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Node.Stream (onFinish)
import Prelude (bind, map, pure, (#), ($), (*), (-), (==), (>>>))
import Util (readFileLines)

day3Part1 :: Effect (Either String Int)
day3Part1 = do
  input <- readFileLines "test/day3/input.txt"
  let eitherRegex = regex "#(\\d)\\s@\\s(\\d),(\\d):\\s(\\d)x(\\d)" noFlags
  pure case eitherRegex of
    Left error -> Left error
    Right r ->
      let 
        claims = map (match r) input
      in
        Right 1

data Claim = Claim 
  { id :: Int, 
  , x :: Int
  , y :: Int
  , width :: Int
  , height :: Int
  }
      
matchToClaim :: Maybe (NonEmpty.NonEmptyArray (Maybe String)) -> Maybe Claim
matchToClaim Nothing = Nothing
matchToClaim (Just matches) =
  let
    matchesArray = NonEmpty.toArray matches
  in
    case matchesArray of
      [(Just _), (Just id), (Just x), (Just y), (Just width), (Just height)] -> 
        do
          id' <- fromString id
          x' <- fromString x
          y' <- fromString y
          width' <- fromString width
          height' <- fromString height
          pure $ Claim {
            id: id',
            x: x',
            y: y',
            width: width',
            height: height'
          }
      _ -> Nothing
