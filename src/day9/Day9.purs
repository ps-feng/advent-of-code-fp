module Day9 
  ( day9Part1
  , solve2
  , FocusedArray(..)
  , class CircularList, current, right, left, insert, remove
  ) where

import Prelude

import Data.Array ((..), (!!))
import Data.Array (deleteAt, insertAt, length, modifyAt, null) as A
import Data.Char (toCharCode)
import Data.Foldable (foldl)
import Data.List ((:))
import Data.List (List(..), difference, fromFoldable, length, mapMaybe) as L
import Data.Map (Map, alter, empty, keys, lookup, values) as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Ord (abs)
import Data.PQueue (PQueue, empty, fromFoldable, head, insert, tail) as PQ
import Data.Set (difference, empty, fromFoldable, insert, map) as S
import Data.String.CodeUnits (charAt, fromCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Util (readFileLines)

import Debug.Trace (trace)

type Scores = M.Map Int Int

day9Part1 :: Int -> Int -> Effect Int
day9Part1 numPlayers numMarbles = do
  pure $ solve1 numPlayers numMarbles

class CircularList l where
  current :: forall a. l a -> Maybe a
  right :: forall a. l a -> l a
  left :: forall a. l a -> l a
  insert :: forall a. a -> l a -> l a
  remove :: forall a. l a -> l a

-- instance showCircularList :: (Show l, CircularList l) => Show l where
--   show _ = ""

-- This is pretty inefficient and doesn't solve Part 2 in a reasonable time.
-- Ideally we'd have some sort of doubly-linked list or Zipper structure.
newtype FocusedArray a = FocusedArray
  { currentIndex :: Int
  , array :: Array a
  }

instance showFocusedArray :: (Show a) => Show (FocusedArray a) where
  show (FocusedArray { currentIndex, array }) = "<focus: " <> show currentIndex <> ", list: " <> show array <> ">"

instance arrayCircularList :: CircularList FocusedArray where
  current (FocusedArray { currentIndex, array }) = array !! currentIndex
  right (FocusedArray current@{ currentIndex, array }) = FocusedArray current { currentIndex = (currentIndex + 1) `mod` A.length array }
  left (FocusedArray current@{ currentIndex, array }) = FocusedArray current { currentIndex = (currentIndex - 1) `mod` A.length array }
  insert elem (FocusedArray { currentIndex, array }) = 
    let nextIndex = if A.null array then 0 else currentIndex + 1
    in FocusedArray 
        { currentIndex: nextIndex
        , array: fromMaybe array $ A.insertAt nextIndex elem array 
        }
  remove (FocusedArray current@{ currentIndex, array }) = 
    let
      newArray = fromMaybe array $ A.deleteAt currentIndex array 
    in
      FocusedArray current 
        { currentIndex = currentIndex `mod` A.length newArray
        , array = newArray
        }

moveFocus :: forall f a. CircularList f => Int -> f a -> f a
moveFocus steps l =
  foldl go l (0 .. ((abs steps) - 1))
  where
    go list _ =
      if steps > 0 then right list 
      else left list

type State =
  { player :: Int
  , circle :: FocusedArray Int
  , scores :: Scores
  }

solve1 :: Int -> Int -> Int
solve1 numPlayers numMarbles =
  let
    marbles = 1 .. numMarbles
    initialState =
      { player: 0
      , circle: FocusedArray { currentIndex: 0, array: [0] }
      , scores: M.empty :: Scores}
  in
    maxScore $ foldl go initialState marbles
    where
      go :: State -> Int -> State
      go { player, circle, scores } marble =
        if (marble `mod` 23) /= 0 then
          let
            newCircle = insert marble $ moveFocus 1 circle
            -- bla = trace (show newCircle) \_ -> 1
          in
            { player: nextPlayer, circle: newCircle, scores }
        else
          let
            circleFocusedAtMinus7 = moveFocus (-7) circle
            marbleAtMinus7 = fromMaybe 0 $ current circleFocusedAtMinus7
            newCircle = remove circleFocusedAtMinus7
            -- bla = trace (show newCircle) \_ -> 1
            newScores = M.alter (\v -> Just $ (fromMaybe 0 v) + marbleAtMinus7 + marble) player scores
          in
            { player: nextPlayer, circle: newCircle, scores: newScores }
        where
          nextPlayer = (player + 1) `mod` numPlayers
  
      maxScore :: State -> Int
      maxScore { player: _, circle: _, scores } =
        foldl max 0 (M.values scores)

solve2 :: Int -> Int -> Int
solve2 numPlayers numMarbles =
  let
    --marbles = fromMaybe [] $ A.modifyAt (numMarbles - 1) (\_ -> 100 * numMarbles) $ 1 .. numMarbles
    marbles = 1 .. (numMarbles * 100)
    initialState =
      { player: 0
      , circle: FocusedArray { currentIndex: 0, array: [0] }
      , scores: M.empty :: Scores}
  in
    maxScore $ foldl go initialState marbles
    where
      go :: State -> Int -> State
      go { player, circle, scores } marble =
        if (marble `mod` 23) /= 0 then
          let
            newCircle = insert marble $ moveFocus 1 circle
            -- bla = trace (show newCircle) \_ -> 1
          in
            { player: nextPlayer, circle: newCircle, scores }
        else
          let
            circleFocusedAtMinus7 = moveFocus (-7) circle
            marbleAtMinus7 = fromMaybe 0 $ current circleFocusedAtMinus7
            newCircle = remove circleFocusedAtMinus7
            -- bla = trace (show newCircle) \_ -> 1
            newScores = M.alter (\v -> Just $ (fromMaybe 0 v) + marbleAtMinus7 + marble) player scores
          in
            { player: nextPlayer, circle: newCircle, scores: newScores }
        where
          nextPlayer = (player + 1) `mod` numPlayers
  
      maxScore :: State -> Int
      maxScore { player: _, circle: _, scores } =
        foldl max 0 (M.values scores)
