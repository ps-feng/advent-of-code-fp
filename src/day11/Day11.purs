module Day11 
  ( SerialNumber
  , day11Part1
  ) where

import Prelude

import Data.Array (intercalate, (..))
import Data.Foldable (foldl, maximumBy)
import Data.Function (on)
import Data.Int (decimal, rem, toStringAs)
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Console (log)

day11Part1 :: SerialNumber -> Effect (Maybe Int)
day11Part1 serialNumber = pure $ solve serialNumber

type Coord
  = { x :: Int
    , y :: Int
    }

type Matrix
  = M.Map Coord Int

prettyPrint :: Coord -> Size -> Matrix -> Effect Unit
prettyPrint { x: tlx, y: tly } { width, height } m = 
  log $ intercalate "\n" $ do
  y <- tly .. (tly + height - 1)
  pure $ intercalate "\t" $ do
    x <- tlx .. (tlx + width - 1)
    pure $ fromMaybe "?" $ (toStringAs decimal) <$> M.lookup { x, y } m

type SerialNumber
  = Int

type Size
  = { width :: Int
    , height :: Int
    }

defaultSize :: Size
defaultSize = { width: 300, height: 300 }

-- | Returns `true` if given `Coord` is within the boundaries of a matrix of siz `Size`.
offbounds :: Size -> Coord -> Boolean
offbounds { width, height } { x, y } =
  x <= 0 || y <= 0 || x > width || y > height


-- | Returns the power level at a given `Coord`
powerLevel :: SerialNumber -> Coord -> Int
powerLevel serialNumber { x, y } = 
  let 
    rackId = x + 10
    initialPowerLevel = rackId * y
  in 
    (((initialPowerLevel + serialNumber) * rackId / 100) `rem` 10) - 5

-- | Same as `powerLevel` but it returns `0` if `Coord` is out of bounds.
powerLevel' :: Size -> SerialNumber -> Coord -> Int
powerLevel' s sn c 
  | offbounds s c = 0
  | otherwise = powerLevel sn c

-- | Creates a new matrix of a given `Size` filled with the `powerLevel` for each cell.
-- |
-- | Used only for debugging.
makePowerLevelMatrix :: Size -> SerialNumber -> Matrix
makePowerLevelMatrix { width, height } serialNumber = M.fromFoldable $ do
  x <- 1 .. height
  y <- 1 .. width
  let coord = { x, y }
  pure $ Tuple coord $ powerLevel serialNumber coord

-- | Calculate the `auxiliar` matrix i.e. a matrix where { x, y } contains the
-- | sum of all cells in { 0, 0 }, { x, y }.
makeAuxiliarMatrix :: Size -> SerialNumber -> Matrix
makeAuxiliarMatrix size@{ width, height } serialNumber =
  rowWiseSum $ columnWiseSum
  where
    columnWiseSum = 
      foldl updateMatrixColumnWisePowerLevel M.empty $ do
        x <- 0 .. width
        y <- 0 .. height
        pure $ { x, y }

    updateMatrixColumnWisePowerLevel matrix coord@{ x, y } =
      let
        prevCoord =  { x: x - 1, y }
        prevPowerLevel = fromMaybe (powerLevel' size serialNumber prevCoord) $ M.lookup prevCoord matrix
        currPowerLevel = (powerLevel' size serialNumber coord)
      in
        M.insert coord (currPowerLevel + prevPowerLevel) matrix

    rowWiseSum matrix =
        foldl updateMatrixRowWisePowerLevel matrix $ do
          x <- 0 .. width
          y <- 0 .. height
          pure $ { x, y }

    updateMatrixRowWisePowerLevel matrix coord@{ x, y } =
      let
        prevCoord =  { x, y: y - 1}
        prevPowerLevel = fromMaybe (powerLevel' size serialNumber prevCoord) $ M.lookup prevCoord matrix
      in
        M.update (Just <<< (_ + prevPowerLevel)) coord matrix

-- | Given the `SerialNumber`, it returns the 3x3 matrix which has the largest total power.
solve :: SerialNumber -> Maybe Int
solve serialNumber = 
  let
    { width, height } = defaultSize
    matrix = makeAuxiliarMatrix defaultSize serialNumber
    resultTuple = maximumBy (compare `on` snd) $ do
      x <- 0 .. (width - 2)
      y <- 0 .. (height - 2)
      pure $ Tuple { x, y } $ fromMaybe 0 $ powerLevelSum matrix { x, y }
  in
    snd <$> resultTuple

-- | It returns the total power of a matrix 3x3 that has top left coords equal to `Coord`s.
powerLevelSum :: Matrix -> Coord -> Maybe Int
powerLevelSum matrix { x, y } = 
  let
    topLeft = { x: x - 1, y: y - 1 }
    bottomRight = { x: x + 2, y: y + 2 }
  in
    do
      a <- M.lookup bottomRight matrix
      b <- M.lookup { x: bottomRight.x, y: topLeft.y } matrix
      c <- M.lookup { x: topLeft.x, y: bottomRight.y } matrix
      d <- M.lookup topLeft matrix
      pure $ a - b - c + d