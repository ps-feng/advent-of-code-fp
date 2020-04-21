module Day6Brute (Coord, prettyPrint') where

import Prelude
import Data.Array as A
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.Map as M
import Data.Ord
import Data.String.CodeUnits
import Effect (Effect)
import Effect.Console (log)

type Coord
  = { x :: Int
    , y :: Int
    }

type Location
  = Coord

data CoordState
  = ClosestTo Location Int
  | ClosestToMultipleLocations Int

type Grid
  = M.Map Coord CoordState

type BoundingBox
  = 
  { min :: Coord
  , max :: Coord
  }

makeBoundingBox :: Array Location -> BoundingBox
makeBoundingBox locations = { min: { x: minX, y: minY },  max: { x: maxX, y: maxY } }
  where
  minX = fromMaybe 0 $ (_.x <$> minimumBy (compare `on` _.x) locations)
  minY = fromMaybe 0 $ (_.y <$> minimumBy (compare `on` _.y) locations)
  maxX = fromMaybe 0 $ (_.x <$> maximumBy (compare `on` _.x) locations)
  maxY = fromMaybe 0 $ (_.y <$> maximumBy (compare `on` _.y) locations)

isAtBoundary :: BoundingBox -> Coord -> Boolean
isAtBoundary { min: { x: minX, y: minY}, max: { x: maxX, y: maxY} } coord = 
  coord.x == minX || coord.x == maxX || coord.y == minY || coord.y == maxY

toFoldable :: BoundingBox -> Array Coord
toFoldable bbox = do
  y <- bbox.min.y A... bbox.max.y
  x <- bbox.min.x A... bbox.max.x
  pure $ { x,  y }

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance a b = abs (a.x - b.x) + abs (a.y - b.y)

-- calculateMaxArea :: BoundingBox -> Grid -> Int 
-- calculateMaxArea bbox grid = 
--   let locationsWithInfiniteArea = 
--   foldl (\(Tuple k v) -> ) 0 M.toUnfoldable grid

solve :: Array Coord -> Grid
solve input =
  let 
    bbox = makeBoundingBox input
    bboxCoords = toFoldable bbox
    updateGrid = solve' bboxCoords
  in foldl updateGrid M.empty input

solve' :: Array Coord -> Grid -> Location -> Grid
solve' bboxCoords initialGrid location = 
  foldl (\acc pos ->
    let distance = manhattanDistance location pos
    in M.alter (\v -> Just $ mergeCoordState location distance v) pos acc
  ) initialGrid bboxCoords

mergeCoordState :: Location        
                -> Int               -- Distance
                -> Maybe CoordState  -- The current state
                -> CoordState        -- The new state
mergeCoordState newLocation newDistance Nothing = ClosestTo newLocation newDistance
mergeCoordState newLocation newDistance (Just currentState) =
  let
    currentDist =
      case currentState of
        ClosestTo _ distance -> distance
        ClosestToMultipleLocations distance -> distance
  in
    mergeCoordState' currentDist
  where
    mergeCoordState' currentDistance 
      | newDistance == currentDistance = ClosestToMultipleLocations currentDistance
      | newDistance < currentDistance  = ClosestTo newLocation newDistance
      | otherwise                      = currentState

prettyPrint' :: Array Location -> Effect Unit
prettyPrint' locations = 
  do
    A.fold $ do
      y <- bbox.min.y A... bbox.max.y
      let row = map (flip locationToString y) $ bbox.min.x A... (bbox.max.x)
      pure $ log $ intercalate " | " row
  where
    bbox = makeBoundingBox locations
    locationsToLetter = M.fromFoldable $ A.zip locations (toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    grid = solve locations
    locationToString x y = case M.lookup {x, y} grid of
      Just (ClosestTo location _) -> singleton $ fromMaybe '!' $ M.lookup location locationsToLetter
      Just (ClosestToMultipleLocations _) -> "."
      Nothing -> "?"