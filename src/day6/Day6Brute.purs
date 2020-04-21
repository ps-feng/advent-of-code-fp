module Day6Brute (Coord, day6Part2, solve) where

import Prelude

import Data.Array as A
import Data.Foldable (foldl, maximumBy, minimumBy)
import Data.Function (on)
import Data.Int (fromString)
import Data.List as L
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (abs)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Partial.Unsafe (unsafePartial)
import Util (readFileLines)

type Coord
  = { x :: Int
    , y :: Int
    }

type Location
  = Coord

type Grid
  = M.Map Coord Int

type BoundingBox
  = 
  { min :: Coord
  , max :: Coord
  }

type Distance = Int

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

gridCoordsFromBoundingBox :: BoundingBox -> Array Coord
gridCoordsFromBoundingBox bbox = do
  y <- bbox.min.y A... bbox.max.y
  x <- bbox.min.x A... bbox.max.x
  pure $ { x,  y }

manhattanDistance :: Coord -> Coord -> Int
manhattanDistance a b = abs (a.x - b.x) + abs (a.y - b.y)

day6Part2 :: Distance -> String -> Effect (Maybe Int)
day6Part2 distance filePath = do
  input <- readFileLines filePath
  pure
    $ do
        locations <- traverse splitLineToCoord input
        pure $ solve distance locations
  where
    splitLineToCoord :: String -> Maybe Coord
    splitLineToCoord input = splitLineToCoord' $ split (Pattern ", ") input
      where
      splitLineToCoord' [ x, y ] = makeCoord <$> (fromString x) <*> (fromString y)
        where
          makeCoord :: Int -> Int -> Coord
          makeCoord x' y' = { x: x', y: y' }

      splitLineToCoord' _ = Nothing

solve :: Distance -> Array Location -> Int
solve maximumDistance locations =
  let 
    bbox = makeBoundingBox locations
    gridCoords = gridCoordsFromBoundingBox bbox
    finalGrid = foldl (updateCoord maximumDistance locations)  M.empty gridCoords
  in
    L.length $ L.filter (_ < maximumDistance) $ M.values finalGrid
  where
    -- updateGridForCoord :: Grid -> Coord -> Grid
    -- updateGridForCoord initialGrid coord = 
    --   foldl (\acc location ->
    --     let distance = manhattanDistance coord location
    --     in M.alter (\v -> Just $ (fromMaybe 0 v) + distance) coord acc
    --   ) initialGrid locations

    -- This was supposed to be faster by not continue to calculate the distance when
    -- it's over the maximum, but still superslow...
    updateCoord :: Distance -> Array Location -> Grid -> Coord -> Grid
    updateCoord maxDistance locations' grid coord =
      if A.null locations' then grid
      else
        let
          location = unsafePartial $ A.unsafeIndex locations' 0
          locationsTail = fromMaybe [] $ A.tail locations'
          distance = manhattanDistance coord location
          saveDist = fromMaybe 0 $ M.lookup coord grid
          newDist = distance + saveDist
          newGrid = M.insert coord newDist grid
          remainingLocations = if newDist > maxDistance then [] else locationsTail
        in
          updateCoord maxDistance remainingLocations newGrid coord

-- prettyPrint' :: Distance -> Array Location -> Effect Unit
-- prettyPrint' maximumDistance locations = 
--   do
--     A.fold $ do
--       y <- bbox.min.y A... bbox.max.y
--       let row = map (flip locationToString y) $ bbox.min.x A... (bbox.max.x)
--       pure $ log $ intercalate " | " row
--   where
--     bbox = makeBoundingBox locations
--     grid = solve maximumDistance locations
--     locationToString x y = case M.lookup {x, y} grid of
--       Just distance | distance < maximumDistance -> "#"
--       Just distance -> " "
--       Nothing -> " "