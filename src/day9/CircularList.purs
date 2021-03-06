module Day9.CircularList 
  ( class CircularList
  , FocusedArray(..)
  , current
  , insert
  , left
  , moveFocus
  , remove
  , right
  ) where

import Prelude

import Data.Array (deleteAt, insertAt, length, null) as A
import Data.Array (foldl, (!!), (..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Ord (abs)
import Day9.ListZipper (ListZipper)
import Day9.ListZipper (beginp, delete, endp, insert, left, right, safeCursor) as LZ

class CircularList l where
  current :: forall a. l a -> Maybe a
  right :: forall a. l a -> l a
  left :: forall a. l a -> l a
  insert :: forall a. a -> l a -> l a
  remove :: forall a. l a -> l a

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

instance circularListZipper :: CircularList ListZipper where

  current zipper = LZ.safeCursor zipper

  right zipper =     
    let r = fromMaybe zipper $ LZ.right zipper
    in if LZ.endp r then (top r) else r

  left zipper = 
    if LZ.beginp zipper 
    then bottom zipper
    else fromMaybe zipper $ LZ.left zipper
    
  insert a zipper = 
    let a' = LZ.insert a $ fromMaybe zipper $ LZ.right zipper
    in if LZ.endp a' then (top a') else a'

  remove a = 
    let a' = LZ.delete a
    in if LZ.endp a' then (top a') else a'

-- Private 

top :: forall a. ListZipper a -> ListZipper a
top a 
  | LZ.beginp a = a
  | otherwise = top $ fromMaybe a $ LZ.left a 

bottom :: forall a. ListZipper a -> ListZipper a
bottom a   
  | LZ.endp a = fromMaybe a $ LZ.left a
  | otherwise = bottom $ fromMaybe a $ LZ.right a