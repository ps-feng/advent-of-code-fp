module Day5
  ( day5Part1
  , day5Part2
  ) where

import Data.Array (fromFoldable)
import Data.Foldable (fold, foldr, minimumBy)
import Data.Function (on)
import Data.List as L
import Data.List.Types ((:))
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String as String
import Data.String.Common (toLower)
import Data.String.Utils (filter, toCharArray)
import Data.Traversable (traverse)
import Effect (Effect)
import Prelude
import Util (readTextFile)

day5Part1 :: String -> Effect Int
day5Part1 filePath = solve filePath regularPolymerReactor

day5Part2 :: String -> Effect Int
day5Part2 filePath = solve filePath shortestPolymerReactor

type Reactor
  = (String -> Maybe String)

solve :: String -> Reactor -> Effect Int
solve filePath reactor = do
  inputPolymer <- readTextFile filePath
  case reactor inputPolymer of
    Nothing -> pure (-1)
    Just polymer -> pure $ String.length polymer

regularPolymerReactor :: Reactor
regularPolymerReactor = polymerReactor Nothing

uniqueChars :: String -> Array String
uniqueChars input =
  toLower input
    # toCharArray
    # S.fromFoldable
    # fromFoldable

shortestPolymerReactor :: Reactor
shortestPolymerReactor input =
  let
    charsToIgnore = uniqueChars input

    reactions :: Maybe (Array String)
    reactions = traverse (\char -> polymerReactor (Just char) input) charsToIgnore
  in
    case reactions of
      Nothing -> Nothing
      Just strings -> minimumBy (compare `on` String.length) strings

polymerReactor :: Maybe String -> Reactor
polymerReactor maybeCharToIgnore polymer =
  let
    filteredPolymer = case maybeCharToIgnore of
      Nothing -> polymer
      Just charToIgnore -> filter (\c -> toLower c /= toLower charToIgnore) polymer

    polymerArr = toCharArray filteredPolymer
  in
    --runSlowAlgorithm polymerArr -- this was the first slow version using ZipperArray
    Just $ runFastAlgorithm polymerArr

runFastAlgorithm :: Array String -> String
runFastAlgorithm polymerArr = polymerReactorFast $ L.fromFoldable polymerArr

-- Inspired by https://github.com/glguy/advent2018/blob/master/execs/Day05.hs
polymerReactorFast :: L.List String -> String
polymerReactorFast polymer =
  let
    str = foldr step (L.singleton "") polymer
  in
    fold str
  where
  step x (y : ys)
    | canReact x y = ys

  step x ys = x : ys

canReact :: String -> String -> Boolean
canReact a b =
  if a == b then
    false
  else
    toLower a == toLower b

-- FIRST SOLUTION: VERY SLOW, uses ZipperArray
-- runSlowAlgorithm :: Array String -> Maybe String
-- runSlowAlgorithm polymerArr = do
--   initialZipper <- Z.fromArray polymerArr
--   finalZipper <- polymerReactor' initialZipper
--   pure $ fromCharArray $ Z.toArray finalZipper

-- polymerReactor' :: Z.ZipperArray String -> Maybe (Z.ZipperArray String)
-- polymerReactor' zipper = do
--   case Z.next zipper of
--     Nothing -> Just zipper
--     Just next ->
--       if canReact (Z.current zipper) next then case delete2Chars zipper of
--         Nothing -> Nothing
--         Just newZipper -> polymerReactor' newZipper
--       else case Z.goNext zipper of
--         Nothing -> Just zipper
--         Just newZipper -> polymerReactor' newZipper
--   where
--   delete2Chars =
--     deleteCurrent Z.DeleteFocusedItemFocusBumpPolicyGoNext
--       >=> deleteCurrent Z.DeleteFocusedItemFocusBumpPolicyGoPrev

-- deleteCurrent :: forall a. Z.DeleteFocusedItemFocusBumpPolicy -> Z.ZipperArray a -> Maybe (Z.ZipperArray a)
-- deleteCurrent bumpPolicy zipper = do
--   newZipper <- Z.fromArray $ preceding <> succeeding
--   Z.goIndex newIdx newZipper
--   where
--   preceding = Z.prec zipper

--   succeeding = Z.succ zipper

--   curIdx = natToInt $ Z.curIndex zipper

--   newIdx =
--     intToNat
--       $ case bumpPolicy of
--           Z.DeleteFocusedItemFocusBumpPolicyGoPrev -> max 0 (curIdx - 1)
--           Z.DeleteFocusedItemFocusBumpPolicyGoNext -> min (Z.length zipper - 2) curIdx
