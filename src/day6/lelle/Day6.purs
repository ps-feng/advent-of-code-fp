module Day6.Lelle 
  ( BoundingBox
  , Coord(..)
  , CoordState(..)
  , Grid(..)
  , GridUpdateResult(..)
  , Label
  , Location(..)
  , makeBoundingBoxFromCoords
  , mergeCoordState
  , updateGridForCellsAtDistanceFromLocation
  )
where

import Prelude

import Data.Array (zipWith, (..))
import Data.Foldable (foldl)
import Data.Map (Map, alter)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

-- Types

type Label = String

type Coord =
  { x :: Int
  , y :: Int 
  }

type Location =
    { label :: Label
    , pos :: Coord
    }

data CoordState 
  = OutOfBounds
  | ClosestTo Label Int
  | ClosestToMultipleLocations Int

derive instance eqCoordState :: Eq CoordState
instance showCoordState :: Show CoordState where
  show OutOfBounds = "OutOfBounds"
  show (ClosestTo label distance) = "ClosestTo " <> label <> " " <> show distance
  show (ClosestToMultipleLocations distance) = "ClosestToMultipleLocations " <> show distance

type Grid = Map Coord CoordState

type GridUpdateResult = Tuple Grid Int

type BoundingBox = Coord -> Boolean

type Distance = Int

-- Functions

makeBoundingBoxForLocations :: Array Location  -- The array of location used to derive the bounding box
                            -> BoundingBox     -- The bounding box
makeBoundingBoxForLocations _ _ = false

makeBoundingBoxFromCoords :: Coord -> Coord -> BoundingBox
makeBoundingBoxFromCoords topLeft bottomRight = 
  \coord -> coord.x >= topLeft.x 
         && coord.x <= bottomRight.x
         && coord.y >= topLeft.y
         && coord.y <= bottomRight.y

coordsAtDistance :: Coord -> Array Coord
coordsAtDistance coord = []

updateGridForCellsAtDistanceFromLocation :: BoundingBox      -- The bounding box to calculate if coord is valid (inside)
                                         -> Location         -- The location for which we are updating the grid
                                         -> Distance         -- The distance from the location
                                         -> Grid             -- The current grid
                                         -> GridUpdateResult -- A tuple that contains the new grid and the number of valid changes
updateGridForCellsAtDistanceFromLocation boundingBox location distance grid =
  foldl updateCoordStateAtCoord (Tuple grid 0) $ coordsAtDistanceFromCoord location.pos distance
  where
    updateCoordStateAtCoord (Tuple theGrid numberOfValidUpdates) coord =
      let 
        newState = if boundingBox coord then (ClosestTo location.label distance) else OutOfBounds
        theGrid' = alter (\v -> Just $ mergeCoordState newState v) coord theGrid
        numberOfValidUpdates' = 
          case newState of
            (ClosestTo _ _) -> numberOfValidUpdates + 1
            _ -> numberOfValidUpdates
      in Tuple theGrid' numberOfValidUpdates'
            
mergeCoordState :: CoordState        -- The state
                -> Maybe CoordState  -- The state to merge into (if any)
                -> CoordState        -- The merged state
mergeCoordState state Nothing = state
mergeCoordState _ (Just OutOfBounds) = OutOfBounds
mergeCoordState OutOfBounds _ = OutOfBounds
mergeCoordState state (Just current) =
  case [ state, current ] of
    [ ClosestTo llabel ldistance,  ClosestTo rlabel rdistance ] -> 
      if ldistance == rdistance && llabel == rlabel
      then current
      else mergeCoordState' ldistance rdistance
    [ ClosestToMultipleLocations ldistance,  ClosestTo _ rdistance ] -> mergeCoordState' ldistance rdistance
    [ ClosestTo _ ldistance,  ClosestToMultipleLocations rdistance ] -> mergeCoordState' ldistance rdistance
    [ ClosestToMultipleLocations ldistance,  ClosestToMultipleLocations rdistance ] -> mergeCoordState' ldistance rdistance
    _ -> OutOfBounds -- Only to hush the compiler, this would never happens
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
        xs = 0 .. distanceMinusOne <> distance .. negateDistance <> negateDistanceMinusOne .. (-1)
        ys = negateDistance .. distanceMinusOne <> distance .. negateDistanceMinusOne
      in
        zipWith (\x y -> { x: x + rx, y: y + ry }) xs ys
