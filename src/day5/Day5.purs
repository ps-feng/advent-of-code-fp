module Day5
  ( day5Part1
  ) where

import Data.Natural
import Debug.Trace
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
import Data.String as String
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

day5Part1 :: String -> Effect Int
day5Part1 filePath = solve filePath part1Reactor

type Reactor
  = (String -> Maybe String)

solve :: String -> Reactor -> Effect Int
solve filePath reactor = do
  inputPolymer <- readTextFile filePath
  case reactor inputPolymer of
    Nothing -> pure (-1)
    Just polymer -> pure $ String.length polymer

part1Reactor :: String -> Maybe String
part1Reactor polymer = do
  initialZipper <- Z.fromArray $ toCharArray polymer
  finalZipper <- part1Reactor' initialZipper
  pure $ fromCharArray $ Z.toArray finalZipper

part1Reactor' :: Z.ZipperArray String -> Maybe (Z.ZipperArray String)
part1Reactor' zipper = do
  case Z.next zipper of
    Nothing -> Just zipper
    Just next ->
      if canReact (Z.current zipper) next then case delete2Chars zipper of
        Nothing -> Nothing
        Just newZipper -> part1Reactor' newZipper
      else case Z.goNext zipper of
        Nothing -> Just zipper
        Just newZipper -> part1Reactor' newZipper
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
deleteCurrent bumpPolicy zipper = do
  newZipper <- Z.fromArray $ preceding <> succeeding
  Z.goIndex newIdx newZipper
  where
  preceding = Z.prec zipper

  succeeding = Z.succ zipper

  curIdx = natToInt $ Z.curIndex zipper

  newIdx =
    intToNat
      $ case bumpPolicy of
          Z.DeleteFocusedItemFocusBumpPolicyGoPrev -> max 0 (curIdx - 1)
          Z.DeleteFocusedItemFocusBumpPolicyGoNext -> min (Z.length zipper - 2) curIdx
