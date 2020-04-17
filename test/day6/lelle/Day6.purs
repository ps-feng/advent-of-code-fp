module Test.Day6.Lelle where

import Day6.Lelle
  ( CoordState(..)
  , makeBoundingBoxFromCoords
  , mergeCoordState
  , updateGridForCellsAtDistanceFromLocation
  )
import Prelude (Unit, discard, negate, ($))
import Data.Map (empty, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Test.Assert (assertEqual, assertFalse, assertTrue)

main :: Effect Unit
main = do
  testUpdateGridForCellsAtDistanceFromLocation
  testUpdateGridForCellsAtDistanceFromLocationOverBoundaries
  testMakeBoundingBoxFromCoords
  testMergeCoordState

testUpdateGridForCellsAtDistanceFromLocation :: Effect Unit
testUpdateGridForCellsAtDistanceFromLocation =
  let
    boundingBox = makeBoundingBoxFromCoords { x: 0, y: 0 } { x: 2, y: 2 }

    location = { label: "A", pos: { x: 1, y: 1 } }

    updateStatus = updateGridForCellsAtDistanceFromLocation boundingBox location 1 empty

    expected =
      fromFoldable
        $ [ Tuple { x: 1, y: 0 } (ClosestTo "A" 1)
          , Tuple { x: 2, y: 1 } (ClosestTo "A" 1)
          , Tuple { x: 1, y: 2 } (ClosestTo "A" 1)
          , Tuple { x: 0, y: 1 } (ClosestTo "A" 1)
          ]
  in
    do
      assertEqual { actual: snd updateStatus, expected: 4 }
      assertEqual { actual: fst updateStatus, expected: expected }

testUpdateGridForCellsAtDistanceFromLocationOverBoundaries :: Effect Unit
testUpdateGridForCellsAtDistanceFromLocationOverBoundaries =
  let
    boundingBox = makeBoundingBoxFromCoords { x: 0, y: 0 } { x: 2, y: 2 }

    location = { label: "A", pos: { x: 0, y: 0 } }

    updateStatus = updateGridForCellsAtDistanceFromLocation boundingBox location 1 empty

    expected =
      fromFoldable
        $ [ Tuple { x: 0, y: -1 } OutOfBounds
          , Tuple { x: 1, y: 0 } (ClosestTo "A" 1)
          , Tuple { x: 0, y: 1 } (ClosestTo "A" 1)
          , Tuple { x: -1, y: 0 } OutOfBounds
          ]
  in
    do
      assertEqual { actual: snd updateStatus, expected: 2 }
      assertEqual { actual: fst updateStatus, expected: expected }

testMakeBoundingBoxFromCoords :: Effect Unit
testMakeBoundingBoxFromCoords =
  let
    isInBoundingBox = makeBoundingBoxFromCoords { x: 0, y: 0 } { x: 100, y: 100 }
  in
    do
      assertTrue $ isInBoundingBox { x: 0, y: 0 }
      assertTrue $ isInBoundingBox { x: 100, y: 100 }
      assertFalse $ isInBoundingBox { x: -1, y: -1 }
      assertFalse $ isInBoundingBox { x: 101, y: 101 }

testMergeCoordState :: Effect Unit
testMergeCoordState = do
  assertMergeCoord OutOfBounds Nothing OutOfBounds
  assertMergeCoord (ClosestTo "A" 1) Nothing (ClosestTo "A" 1)
  assertMergeCoord (ClosestToMultipleLocations 1) Nothing (ClosestToMultipleLocations 1)
  assertMergeCoord OutOfBounds (Just OutOfBounds) OutOfBounds
  assertMergeCoord (ClosestTo "A" 1) (Just OutOfBounds) OutOfBounds
  assertMergeCoord (ClosestToMultipleLocations 1) (Just OutOfBounds) OutOfBounds
  assertMergeCoord OutOfBounds (Just (ClosestTo "A" 1)) OutOfBounds
  assertMergeCoord OutOfBounds (Just (ClosestToMultipleLocations 1)) OutOfBounds
  assertMergeCoord (ClosestTo "A" 1) (Just (ClosestTo "A" 1)) (ClosestTo "A" 1)
  assertMergeCoord (ClosestTo "A" 2) (Just (ClosestTo "A" 1)) (ClosestTo "A" 1)
  assertMergeCoord (ClosestTo "A" 1) (Just (ClosestTo "A" 2)) (ClosestTo "A" 1)
  assertMergeCoord (ClosestTo "A" 1) (Just (ClosestTo "B" 1)) (ClosestToMultipleLocations 1)
  assertMergeCoord (ClosestTo "A" 2) (Just (ClosestTo "B" 1)) (ClosestTo "B" 1)
  assertMergeCoord (ClosestTo "A" 1) (Just (ClosestTo "B" 2)) (ClosestTo "A" 1)
  assertMergeCoord (ClosestTo "A" 1) (Just (ClosestToMultipleLocations 1)) (ClosestToMultipleLocations 1)
  assertMergeCoord (ClosestTo "A" 2) (Just (ClosestToMultipleLocations 1)) (ClosestToMultipleLocations 1)
  assertMergeCoord (ClosestTo "A" 1) (Just (ClosestToMultipleLocations 2)) (ClosestTo "A" 1)

-- Asserts
assertMergeCoord :: CoordState -> Maybe CoordState -> CoordState -> Effect Unit
assertMergeCoord lhs rhs expected =
  let
    actual = mergeCoordState lhs rhs
  in
    assertEqual { actual: actual, expected: expected }
