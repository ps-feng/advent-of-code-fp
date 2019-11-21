module Day5
  ( day5Part1
  , canReact
  , deleteCurrent
  ) where

import Prelude
import Data.Array (concat, group, insert, length, range, sort)
import Data.Array.NonEmpty as NonEmpty
import Data.DateTime (DateTime(..), time)
import Data.Either (Either, hush, note)
import Data.Enum (fromEnum)
import Data.Foldable (foldl, maximumBy)
import Data.Formatter.DateTime (Formatter, parseFormatString, unformat)
import Data.Function (on)
import Data.Int (fromString)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Natural
import Data.String.Common (toLower)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (startsWith, fromCharArray, toCharArray)
import Data.Time (minute)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.ZipperArray as Z
import Effect (Effect)
import Util (readTextFile)

day5Part1 :: String -> Effect String
day5Part1 filePath = solve filePath part1Reactor

type Reactor
  = (String -> Maybe String)

solve :: String -> Reactor -> Effect String
solve filePath reactor = do
  inputPolymer <- readTextFile filePath
  case reactor inputPolymer of
    Nothing -> pure "Error reacting polymer"
    Just polymer -> pure polymer

part1Reactor :: String -> Maybe String
part1Reactor polymer = do
  initialZipper <- Z.fromArray $ toCharArray polymer
  finalZipper <- part1Reactor' initialZipper
  pure $ fromCharArray $ Z.toArray finalZipper

part1Reactor' :: Z.ZipperArray String -> Maybe (Z.ZipperArray String)
part1Reactor' zipper = do
  let
    cur = Z.current zipper
  next <- Z.next zipper
  if canReact cur next then case delete2Chars zipper of
    Just newZipper -> part1Reactor' newZipper
    Nothing -> Nothing
  else case Z.goNext zipper of
    Just newZipper -> part1Reactor' newZipper
    Nothing -> Just zipper
  where
  delete2Chars =
    deleteCurrent Z.DeleteFocusedItemFocusBumpPolicyGoNext
      >=> deleteCurrent Z.DeleteFocusedItemFocusBumpPolicyGoPrev

canReact :: String -> String -> Boolean
canReact a b =
  if a == b then
    false
  else
    toLower a == toLower b

deleteCurrent :: forall a. Z.DeleteFocusedItemFocusBumpPolicy -> Z.ZipperArray a -> Maybe (Z.ZipperArray a)
deleteCurrent bumpPolicy zipper =
  let
    curIdx = natToInt $ Z.curIndex zipper

    preceding = Z.prec zipper

    succeeding = Z.succ zipper

    newIdx =
      intToNat
        $ case bumpPolicy of
            Z.DeleteFocusedItemFocusBumpPolicyGoPrev -> max 0 (curIdx - 1)
            Z.DeleteFocusedItemFocusBumpPolicyGoNext -> min (Z.length zipper - 2) curIdx
  in
    do
      newZipper <- Z.fromArray $ preceding <> succeeding
      Z.goIndex newIdx newZipper
