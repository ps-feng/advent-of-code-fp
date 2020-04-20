module Day6
  ( day6Part1
  , day6Part2
  , fillGrid
  , isInBoundingBox
  , Coord(..)
  , CoordState(..)
  , Location(..)
  , makeCoord
  , prettyPrint
  ) where

import Prelude

import Effect.Console (log)

import Data.Array ((..), drop, fold, insert, length, snoc, take, zipWith) as A
import Data.Foldable (foldl, maximumBy)
import Data.Function (on)
import Data.Int (fromString)
import Data.Map (Map, empty, insert, lookup, member, toUnfoldable, values)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set, empty, fromFoldable, insert, member) as S
import Data.String.Common (split)
import Data.String.Pattern (Pattern(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Util (readFileLines)
import Debug.Trace (trace)

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

type MaxAreaAcc =
  { infiniteLocations :: S.Set Location
  , nonInfiniteLocationToCount :: Map Location Int
  }
  
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
  = Map Coord CoordState

type GridUpdateResult
  = Tuple Grid Int

type Distance
  = Int

type BoundingBox
  = 
  { maxX :: Int
  , maxY :: Int 
  }

type Solver
  = (Array Location -> Int)

day6Part1 :: String -> Effect (Maybe Int)
day6Part1 filePath = solve filePath (\_ -> 1)

day6Part2 :: String -> Effect (Maybe Int)
day6Part2 filePath = solve filePath (\_ -> 1)

solve :: String -> Solver -> Effect (Maybe Int)
solve filePath solver = do
  input <- readFileLines filePath
  pure
    $ do
        locations <- traverse splitLineToCoord input
        pure $ solve' locations

splitLineToCoord :: String -> Maybe Coord
splitLineToCoord input = splitLineToCoord' $ split (Pattern ", ") input
  where
  splitLineToCoord' [ x, y ] = makeCoord <$> (fromString x) <*> (fromString y)

  splitLineToCoord' _ = Nothing

solve' :: Array Location -> Int
solve' locations =
  let
    bbox = boundingBox locations
  in
    maxArea bbox $ fillGrid bbox locations

makeCoord :: Int -> Int -> Coord
makeCoord x y = { x: x, y: y }

isInBoundingBox :: Int -> Int -> Coord -> Boolean
isInBoundingBox maxX maxY coord =
  0 <= coord.x && coord.x < maxX &&
  0 <= coord.y && coord.y < maxY

isAtBoundary :: Int -> Int -> Coord -> Boolean
isAtBoundary maxX maxY coord =
  coord.x == 0 || coord.x == maxX - 1 || coord.y == 0 || coord.y == maxY - 1

boundingBox :: Array Coord -> BoundingBox
boundingBox locations = { maxX: maxX, maxY: maxY }
  where
  maxX = fromMaybe 0 $ (_ + 1) <$> (_.x <$> maximumBy (compare `on` _.x) locations)

  maxY = fromMaybe 0 $ (_ + 1) <$> (_.y <$> maximumBy (compare `on` _.y) locations)

-- Fill the grid
-- let setOfInfiniteLocations
-- let mapOfNonInfiniteLocationToCount
-- 
-- For each coord in grid do:
--   if in setOfInfiniteLocations then continue
--   else if coord.isAtBoundary then insert coord setOfInfiniteLocations
--   else alter (+1) coord mapOfNonInfiniteLocationToCount
-- 
-- maxBy { k, v -> v} mapOfNonInfiniteLocationToCount

maxArea :: BoundingBox -> Grid -> Int
maxArea bbox grid =
  let 
    infiniteLocations = S.empty
    nonInfiniteLocationToCount = empty
    gridKeyValues = toUnfoldable grid :: Array (Tuple Coord CoordState)
    { infiniteLocations: _, nonInfiniteLocationToCount } = 
      foldl go { infiniteLocations: S.empty, nonInfiniteLocationToCount: empty } gridKeyValues
        where
          isAtBoundary' = isAtBoundary bbox.maxX bbox.maxY

          go :: MaxAreaAcc -> Tuple Coord CoordState -> MaxAreaAcc
          go acc@{ infiniteLocations, nonInfiniteLocationToCount } (Tuple coord _) =
            if S.member coord infiniteLocations then acc
            else if isAtBoundary' coord then acc { infiniteLocations = S.insert coord infiniteLocations }
            else
              case lookup coord nonInfiniteLocationToCount of
                Just count -> acc { nonInfiniteLocationToCount = insert coord (count + 1) nonInfiniteLocationToCount}
                Nothing -> acc { nonInfiniteLocationToCount = insert coord 1 nonInfiniteLocationToCount}
  in
    foldl max 0 (values nonInfiniteLocationToCount)

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
          chunked' leftOver intermediateArray 

prettyPrint :: Array Location -> Effect Unit
prettyPrint locations = 
  do
    let bbox = boundingBox locations
    let grid = fillGrid bbox locations
    let list = 
          do
            y <- 0 A... (bbox.maxY - 1)
            x <- 0 A... (bbox.maxX - 1)
            let v = lookup { x, y } grid
            pure $ case v of
              Just (ClosestTo location _) -> show location.x <> show location.y
              Just (ClosestToMultipleLocations _) -> "."
              Nothing -> "?"
    let lala = trace "lala" \_ -> chunked list (bbox.maxY - 1) :: Array (Array String)
    let lines = map (foldl (\acc str -> acc <> ", " <> str) "") lala :: Array (String)
    let loggedLines = map log lines :: Array (Effect Unit)
    A.fold loggedLines

-- prettyPrint' :: Array Location -> Effect Unit
-- prettyPrint' locations = 
--   do
--     let bbox = boundingBox locations
--     let grid = fillGrid bbox locations
--     let list = 
--           do
--             y <- 0 .. (bbox.maxY - 1)
--             let v = map (\x -> log $ show x
--              -- lookup { x, y } grid
--               -- log $ case lookup {x, y} grid of
--               --   Just (ClosestTo location _) -> show location.x <> "_" <> show location.y
--               --   Just (ClosestToMultipleLocations _) -> "."
--               --   Nothing -> "?"
--             ) 0 .. (bbox.maxX - 1)
--             pure $ v
--     fold list

fillGrid :: BoundingBox -> Array Location -> Grid
fillGrid bbox locations =
  let
    isInBoundingBox' = isInBoundingBox bbox.maxX bbox.maxY
    locationSet = S.fromFoldable locations
  in
    fillGrid' locationSet 1 isInBoundingBox' empty

fillGrid' :: S.Set Location -> Distance -> (Coord -> Boolean) -> Grid -> Grid
fillGrid' locations distance isInBoundingBox' currentGrid
  | locations == S.empty = currentGrid
  | otherwise =
      let
        Tuple newGrid newLocations = fillGrid'' locations distance isInBoundingBox' currentGrid
      in
        fillGrid' newLocations (distance + 1) isInBoundingBox' newGrid

fillGrid'' :: S.Set Location -> Distance -> (Coord -> Boolean) -> Grid -> Tuple Grid (S.Set Location)
fillGrid'' locations distance isInBoundingBox' currentGrid =
  foldl go (Tuple currentGrid S.empty) locations
  where
    go acc location =
      let 
        curUpdatableLocations = snd acc
        (Tuple updatedGrid numberOfValidUpdates) = updateGridForCellsAtDistanceFromLocation isInBoundingBox' location distance (fst acc)
      in case numberOfValidUpdates of
        0 -> Tuple updatedGrid curUpdatableLocations
        _ -> Tuple updatedGrid (S.insert location curUpdatableLocations)

updateGridForCellsAtDistanceFromLocation :: (Coord -> Boolean)  -- The bounding box to calculate if coord is valid (inside)
                                         -> Location            -- The location for which we are updating the grid
                                         -> Distance            -- The distance from the location
                                         -> Grid                -- The current grid
                                         -> GridUpdateResult    -- A tuple that contains the new grid and the number of valid changes
updateGridForCellsAtDistanceFromLocation isInBoundingBox' location distance grid =
  foldl updateCoordStateAtCoord (Tuple grid 0) $ coordsAtDistanceFromCoord location distance
  where
    updateCoordStateAtCoord all@(Tuple theGrid numberOfValidUpdates) coord 
      | not (isInBoundingBox' coord) = all
      | otherwise =
          let
            currentState = lookup coord theGrid
            newState = mergeCoordState (ClosestTo location distance) currentState
            theGrid' = insert coord newState theGrid
            numberOfValidUpdates' = 
              case newState of
                (ClosestTo l _) | l == location -> numberOfValidUpdates + 1
                _ -> numberOfValidUpdates
          in Tuple theGrid' numberOfValidUpdates'
            
mergeCoordState :: CoordState        -- The state
                -> Maybe CoordState  -- The state to merge into (if any)
                -> CoordState        -- The merged state
mergeCoordState state Nothing = state
mergeCoordState state (Just current) =
  case [ state, current ] of
    [ ClosestTo llocation ldistance,  ClosestTo rlocation rdistance ] -> 
      if llocation == rlocation
      then current
      else mergeCoordState' ldistance rdistance
    [ ClosestToMultipleLocations ldistance, ClosestTo _ rdistance ] -> mergeCoordState' ldistance rdistance
    [ ClosestTo _ ldistance, ClosestToMultipleLocations rdistance ] -> mergeCoordState' ldistance rdistance
    [ ClosestToMultipleLocations ldistance, ClosestToMultipleLocations rdistance ] -> mergeCoordState' ldistance rdistance
    _ -> current -- TODO: check this
  where
    mergeCoordState' stateDistance currentDistance 
      | stateDistance == currentDistance = ClosestToMultipleLocations currentDistance
      | stateDistance < currentDistance  = state
      | otherwise                        = current

coordsAtDistanceFromCoord :: Coord -> Distance -> Array Coord
coordsAtDistanceFromCoord coord 0 = [coord]
coordsAtDistanceFromCoord { x: rx, y: ry } distance = 
  coordsAtDistanceFromCoord'
  where
    coordsAtDistanceFromCoord' =
      let
        distanceMinusOne = (distance - 1)
        negateDistance = negate distance
        negateDistanceMinusOne = negate distanceMinusOne
        xs = 0 A... distanceMinusOne <> distance A... negateDistance <> negateDistanceMinusOne A... (-1)
        ys = negateDistance A... distanceMinusOne <> distance A... negateDistanceMinusOne
      in
        A.zipWith (\x y -> { x: x + rx, y: y + ry }) xs ys
