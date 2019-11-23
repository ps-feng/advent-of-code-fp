module Day6
  ( day6Part1
  , day6Part2
  , isInBoundingBox
  , LocationInfo(..)
  , Coord(..)
  , makeCoord
  , bruteForceCoordinatesFromPositionAtDistance
  , bruteForcePositionAtDistance
  , positionsAtDistance
  , newPositionsAtDistance
  ) where

import Data.Array ((!!), (..), fromFoldable, length, nub, replicate, slice, zip, zipWith)
import Data.Foldable (fold, foldr, maximumBy, minimumBy)
import Data.Function (on)
import Data.Int (fromString)
import Data.List as L
import Data.List.Types ((:))
import Data.Map
import Data.Maybe (Maybe(..), fromMaybe)
import Control.MonadZero
import Data.Set as S
import Data.String as String
import Data.String.Common (toLower, split)
import Data.String.Pattern (Pattern(..))
import Data.String.Utils (filter, toCharArray)
import Data.Traversable (traverse)
import Data.Tuple
import Effect (Effect)
import Prelude
import Util (readFileLines)
import Debug.Trace

day6Part1 :: String -> Effect (Maybe Int)
day6Part1 filePath = solve filePath (\_ -> 1)

day6Part2 :: String -> Effect (Maybe Int)
day6Part2 filePath = solve filePath (\_ -> 1)

type Solver
  = (Array Coord -> Int)

solve :: String -> Solver -> Effect (Maybe Int)
solve filePath solver = do
  input <- readFileLines filePath
  pure $ do
    locations <- traverse splitLineToCoord input
    pure $ solver locations

splitLineToCoord :: String -> Maybe Coord
splitLineToCoord input = splitLineToCoord' $ split (Pattern ", ") input
  where
  splitLineToCoord' [ x, y ] = makeCoord <$> (fromString x) <*> (fromString y)

  splitLineToCoord' _ = Nothing

type Coord
  = 
  { x :: Int
  , y :: Int
  }

makeCoord :: Int -> Int -> Coord
makeCoord x y = { x: x, y: y }

findLargestArea :: Array Coord -> Int
findLargestArea locations = 10
  where
  { maxX: bboxX, maxY: bboxY } = boundingBox locations
  isInBBox = isInBoundingBox bboxX bboxY

isInBoundingBox :: Int -> Int -> Coord -> Boolean
isInBoundingBox maxX maxY coord =
  0 <= coord.x && coord.x < maxX &&
  0 <= coord.y && coord.y < maxY

bruteForcePositionAtDistance :: Int -> Array Coord
bruteForcePositionAtDistance distance = do
  x <- (negate distance) .. distance
  y <- (negate distance) .. distance
  guard $ (abs x) + (abs y)  == distance
  pure { x: x, y: y }
  where 
    abs :: Int -> Int
    abs x = if x >= zero then x else negate x

bruteForceCoordinatesFromPositionAtDistance :: Coord -> Int -> Array Coord
bruteForceCoordinatesFromPositionAtDistance { x: px, y: py } distance =
  map (\{ x, y } -> { x: px + x, y: py + y }) $ bruteForcePositionAtDistance distance

newPositionsAtDistance :: Coord -> Int -> Array Coord
newPositionsAtDistance coord 0 = [coord]
newPositionsAtDistance { x: rx, y: ry } distance = 
  newPositionsAtDistance'
  where
    newPositionsAtDistance' =
      let
        distanceMinusOne = (distance - 1)
        negateDistance = negate distance
        negateDistanceMinusOne = negate distanceMinusOne
        xs = 0 .. distanceMinusOne <> distance .. negateDistance <> negateDistanceMinusOne .. (-1)
        ys = negateDistance .. distanceMinusOne <> distance .. negateDistanceMinusOne
      in
        zipWith (\x y -> { x: x + rx, y: y + ry }) xs ys

boundingBox :: Array Coord -> { maxX :: Int, maxY :: Int}
boundingBox locations =
  { maxX: maxX, maxY: maxY}
  where
  maxX = fromMaybe 0 $ (_+1) <$> (_.x <$> maximumBy (compare `on` _.x) locations)
  maxY = fromMaybe 0 $ (_+1) <$> (_.y <$> maximumBy (compare `on` _.y) locations)

data LocationInfo 
  = Unvisited
  | ClosestTo Int String
  | Equidistant Int

instance showLocationInfo :: Show LocationInfo where
  show Unvisited = "Unvisited"
  show (ClosestTo distance location) = "ClosestTo " <> show distance <> " location " <> show location
  show (Equidistant distance) = "Equidistant " <> show distance

type Grid = Map Coord LocationInfo

fillGrid :: Coord -> Int -> Grid -> Grid
fillGrid coord distance grid =
  grid

positionsAtDistance :: Coord -> Int -> Array Coord
positionsAtDistance location distance =
  let
    -- Given distance 2 for position (0, 0)
    -- ysTop:    0 -1 -2 -1  0
    -- xs:      -2 -1  0  1  2
    -- ysBottom: 0  1  2  1  0
    yRangeBottom = (0 .. distance) <> ((distance - 1) .. 0)
    yRangeTop = map negate yRangeBottom
    xRange = (-distance .. 0) <> (1 .. distance)
    len = length yRangeBottom
    -- first and last elements match with top and bottom, so we slice it
    top = slice 1 (len-1) $ zipWith pairToCoord xRange yRangeTop
    bottom = zipWith pairToCoord xRange yRangeBottom
  in
    top <> bottom
  where
    pairToCoord a b = { x: location.x + a, y: location.y + b }