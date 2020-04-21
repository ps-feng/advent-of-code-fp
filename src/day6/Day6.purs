module Day6
  ( day6Part1
  , fillGrid
  , isInBoundingBox
  , boundingBox
  , maxArea
  , Coord(..)
  , CoordState(..)
  , Location(..)
  , prettyPrint
  , prettyPrint'
  ) where

import Prelude

import Data.Array ((..), drop, fold, length, mapMaybe, snoc, take, zip, zipWith) as A
import Data.Foldable (foldl, intercalate, maximumBy, minimumBy)
import Data.Function (on)
-- import Data.Function.Memoize (memoize)
import Data.Int (fromString)
import Data.Map (Map, empty, insert, lookup, fromFoldable, toUnfoldable, values) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, empty, fromFoldable, insert, isEmpty, member) as S
import Data.String.CodeUnits (singleton, toCharArray)
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Console (log)
import Util (readFileLines)

-- Algorithm
-- 1. Go through each location, one at a time:
--    a. Flood +1 manhattan distance
--    b. If we run into a visited square, update grid with new closest location
--    c. Stop when we can't mark any square anymore
-- 2. Loop through the table keeping
--    - Set of infinite locations (letters whose x=0 or x=table.length or y = 0 or y=table.length)
--    - Count occurrences of each of the locations that are not infinite
-- 3. Stop when we can't continue marking for any of the locations
-- 4. Find out max count
  
type Coord
  = { x :: Int
    , y :: Int
    }

type Location
  = Coord

data CoordState
  = ClosestTo Location Int
  | ClosestToMultipleLocations Int

derive instance eqCoordState :: Eq CoordState

instance showCoordState :: Show CoordState where
  show (ClosestTo location distance) = "ClosestTo " <> show location <> " " <> show distance
  show (ClosestToMultipleLocations distance) = "ClosestToMultipleLocations " <> show distance

type Grid
  = M.Map Coord CoordState

type GridUpdateResult
  = Tuple Grid Int

type Distance
  = Int

type BoundingBox
  = 
  { minX :: Int
  , minY :: Int
  , maxX :: Int
  , maxY :: Int
  }

day6Part1 :: String -> Effect (Maybe Int)
day6Part1 filePath = solve filePath

solve :: String -> Effect (Maybe Int)
solve filePath = do
  input <- readFileLines filePath
  pure
    $ do
        locations <- traverse splitLineToCoord input
        pure $ solve' locations
  where
    splitLineToCoord :: String -> Maybe Coord
    splitLineToCoord input = splitLineToCoord' $ split (Pattern ", ") input
      where
      splitLineToCoord' [ x, y ] = makeCoord <$> (fromString x) <*> (fromString y)
        where
          makeCoord :: Int -> Int -> Coord
          makeCoord x' y' = { x: x', y: y' }

      splitLineToCoord' _ = Nothing

    solve' :: Array Location -> Int
    solve' locations =
      let
        bbox = boundingBox locations
      in
        maxArea bbox $ fillGrid bbox locations

-- Fill the grid:
-- let setOfInfiniteLocations
-- let mapOfNonInfiniteLocationToCount
-- 
-- For each coord in grid do:
--   if in setOfInfiniteLocations then continue
--   else if coord.isAtBoundary then insert coord setOfInfiniteLocations
--   else alter (+1) coord mapOfNonInfiniteLocationToCount
-- 
-- maxBy { k, v -> v} mapOfNonInfiniteLocationToCount
type MaxAreaAcc =
  { infiniteLocations :: S.Set Location
  , nonInfiniteLocationToCount :: M.Map Location Int
  }

maxArea :: BoundingBox -> Grid -> Int
maxArea bbox grid =
  let
    gridKeyValues = M.toUnfoldable grid :: Array (Tuple Coord CoordState)

    -- Goal: all infinite locations
    -- set $ map (value.location) $ filter (keyIsOnTheBoundary) $ toUnfoldable grid
    infiniteLocations :: S.Set Location
    infiniteLocations = 
      S.fromFoldable $ A.mapMaybe go gridKeyValues
        where
          go (Tuple _ (ClosestToMultipleLocations _)) = Nothing
          go (Tuple coord (ClosestTo location _)) = 
            if isAtBoundary bbox coord then Just location
            else Nothing

    nonInfiniteLocationToCount = 
      foldl go M.empty gridKeyValues
        where
          isAtBoundary' = isAtBoundary bbox

          go :: M.Map Location Int -> Tuple Coord CoordState -> M.Map Location Int
          go acc (Tuple coord (ClosestToMultipleLocations _)) = acc
          go acc (Tuple coord (ClosestTo location _))
            | S.member location infiniteLocations = acc
            | otherwise =
                case M.lookup location acc of
                  Just count -> M.insert location (count + 1) acc
                  Nothing -> M.insert location 1 acc
  in
    1 + foldl max 0 (M.values nonInfiniteLocationToCount)

fillGrid :: BoundingBox -> Array Location -> Grid
fillGrid bbox locations =
  fillGrid' initialLocationSet 1 M.empty
  where
    initialLocationSet = S.fromFoldable locations

    fillGrid' :: S.Set Location -> Distance -> Grid -> Grid
    fillGrid' locations' distance currentGrid
      | S.isEmpty locations' = currentGrid
      | otherwise =
          let
            Tuple updatedGrid remainingLocations = fillGrid''
          in
            fillGrid' remainingLocations (distance + 1) updatedGrid
      where
        fillGrid'' :: Tuple Grid (S.Set Location)
        fillGrid'' =
          foldl go (Tuple currentGrid S.empty) locations'
          where
            go acc location =
              let 
                isInBoundingBox' = isInBoundingBox bbox
                curUpdatableLocations = snd acc
                (Tuple updatedGrid numberOfValidUpdates) = updateGridForCellsAtDistanceFromLocation isInBoundingBox' initialLocationSet location distance (fst acc)
              in case numberOfValidUpdates of
                0 -> Tuple updatedGrid curUpdatableLocations
                _ -> Tuple updatedGrid (S.insert location curUpdatableLocations)

updateGridForCellsAtDistanceFromLocation :: (Coord -> Boolean)  -- The bounding box to calculate if coord is valid (inside)
                                         -> S.Set Location      -- Initial set of locations
                                         -> Location            -- The location for which we are updating the grid
                                         -> Distance            -- The distance from the location
                                         -> Grid                -- The current grid
                                         -> GridUpdateResult    -- A tuple that contains the new grid and the number of valid changes
updateGridForCellsAtDistanceFromLocation isInBoundingBox' initialLocations location distance grid =
  foldl updateCoordStateAtCoord (Tuple grid 0) $ coordsAtDistanceFromCoord location distance
  where
    updateCoordStateAtCoord :: GridUpdateResult -> Coord -> GridUpdateResult
    updateCoordStateAtCoord all@(Tuple curGrid numberOfValidUpdates) coord
      | not (isInBoundingBox' coord) = all
      | S.member coord initialLocations = all
      | otherwise =
          let
            currentCoordState = M.lookup coord curGrid
            newState = mergeCoordState location distance currentCoordState
            updatedGrid = M.insert coord newState curGrid
            numberOfValidUpdates' =
              case newState of
                (ClosestTo l _) | l == location -> numberOfValidUpdates + 1
                _ -> numberOfValidUpdates
          in Tuple updatedGrid numberOfValidUpdates'

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

coordsAtDistanceFromCoord :: Coord -> Distance -> Array Coord
coordsAtDistanceFromCoord coord 0 = [coord]
coordsAtDistanceFromCoord { x: rx, y: ry } d = 
  map (\c -> { x: c.x + rx, y: c.y + ry }) $ coordsAtDistanceFromCoord' d
  where
    coordsAtDistanceFromCoord' distance =
      let
        distanceMinusOne = (distance -1)
        negateDistance = negate distance
        negateDistanceMinusOne = negate distanceMinusOne
        xs = 0 A... distanceMinusOne <> distance A... negateDistance <> negateDistanceMinusOne A... (-1)
        ys = negateDistance A... distanceMinusOne <> distance A... negateDistanceMinusOne
      in
        A.zipWith (\x y -> { x: x, y: y }) xs ys

isInBoundingBox :: BoundingBox -> Coord -> Boolean
isInBoundingBox { minX, minY, maxX, maxY } coord =
  minX <= coord.x && coord.x <= maxX &&
  minY <= coord.y && coord.y <= maxY

isAtBoundary :: BoundingBox -> Coord -> Boolean
isAtBoundary { minX, minY, maxX, maxY } coord =
  coord.x == minX || coord.x == maxX || coord.y == minY || coord.y == maxY

boundingBox :: Array Coord -> BoundingBox
boundingBox locations = { minX: minX, minY: minY, maxX: maxX, maxY: maxY }
  where
  minX = fromMaybe 0 $ (_.x <$> minimumBy (compare `on` _.x) locations)
  minY = fromMaybe 0 $ (_.y <$> minimumBy (compare `on` _.y) locations)
  maxX = fromMaybe 0 $ (_.x <$> maximumBy (compare `on` _.x) locations)
  maxY = fromMaybe 0 $ (_.y <$> maximumBy (compare `on` _.y) locations)

chunked :: forall a. Array a -> Int -> Array (Array a)
chunked array len =
  chunked' array []
  where
    chunked' :: Array a -> Array (Array a) -> Array (Array a)
    chunked' arr intermediateArray =
      if A.length arr == 0 then intermediateArray
      else
        let
          newElems = A.take len arr :: Array a
          leftOver = A.drop len arr
          newIntermediateArray = A.snoc intermediateArray newElems
        in
          chunked' leftOver newIntermediateArray 

-- prettyPrint [{x: 1, y: 1}, {x: 1, y: 6}, {x: 8, y: 3}, {x: 3, y: 4}, {x: 5, y: 5}, {x: 8, y: 9}]
prettyPrint :: Array Location -> Effect Unit
prettyPrint locations = 
  do
    let bbox = boundingBox locations
    let grid = fillGrid bbox locations
    let list = 
          do
            y <- 0 A... (bbox.maxY)
            x <- 0 A... (bbox.maxX)
            let v = M.lookup { x, y } grid
            pure $ case v of
              Just (ClosestTo location _) -> "[" <> show location.x <> "," <> show location.y <> "]"
              Just (ClosestToMultipleLocations _) -> "."
              Nothing -> "?"
    let lala = chunked list bbox.maxX :: Array (Array String)
    let lines = map (foldl (\acc str -> acc <> ", " <> str) "") lala :: Array (String)
    let loggedLines = map log lines :: Array (Effect Unit)
    A.fold loggedLines

-- prettyPrint' [{x: 1, y: 1}, {x: 1, y: 6}, {x: 8, y: 3}, {x: 3, y: 4}, {x: 5, y: 5}, {x: 8, y: 9}]
prettyPrint' :: Array Location -> Effect Unit
prettyPrint' locations = 
  do
    A.fold $ do
      y <- bbox.minY A... (bbox.maxY)
      let row = map (flip locationToString y) $ bbox.minX A... (bbox.maxX)
      pure $ log $ intercalate " | " row
  where
    bbox = boundingBox locations
    locationsToLetter = M.fromFoldable $ A.zip locations (toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZ")
    grid = fillGrid bbox locations
    locationToString x y = case M.lookup {x, y} grid of
      Just (ClosestTo location _) -> singleton $ fromMaybe '!' $ M.lookup location locationsToLetter
      Just (ClosestToMultipleLocations _) -> "."
      Nothing -> "?"
