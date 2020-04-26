module Day9.InfiniteListZipper 
  ( InfiniteListZipper
  , empty
  , singleton
  )
where

import Prelude

import Data.Maybe (fromMaybe)
import Day9.CircularList (class CircularList)
import Day9.ListZipper as LZ

-- import Debug.Trace (trace)

-- Types

newtype InfiniteListZipper a = InfiniteListZipper
  { top     :: LZ.ListZipper a
  , bottom  :: LZ.ListZipper a
  , current :: LZ.ListZipper a 
  }

-- Public

empty :: forall a. InfiniteListZipper a
empty = InfiniteListZipper { top: LZ.empty, bottom: LZ.empty, current: LZ.empty }

singleton :: forall a. a -> InfiniteListZipper a
singleton a = 
  let a' = LZ.singleton a
  in InfiniteListZipper { top: a', bottom: a', current: a' }

instance circularListZipper :: CircularList InfiniteListZipper where

  current (InfiniteListZipper { current: c }) = LZ.safeCursor c

  right list@(InfiniteListZipper { top, bottom, current }) =     
    let 
      r = fromMaybe current $ LZ.right current
      r' = if LZ.endp r then top else r
    in 
      InfiniteListZipper { top: top, bottom: bottom, current: r' }

  left list@(InfiniteListZipper { top, bottom, current }) = 
    let l = fromMaybe bottom $ LZ.left current
    in InfiniteListZipper { top: top, bottom: bottom, current: l }

  insert a list@(InfiniteListZipper { current: c }) =
    flip updateWithCurrent list (LZ.insert a $ fromMaybe c $ LZ.right c)

  remove list@(InfiniteListZipper { current: c }) =
    flip updateWithCurrent list $ LZ.delete c

-- Private 

updateWithCurrent :: forall a. LZ.ListZipper a -> InfiniteListZipper a -> InfiniteListZipper a
updateWithCurrent newCurrent (InfiniteListZipper { top: prevTop, bottom: prevBottom, current: prevCurrent }) =
  let
    top = if LZ.beginp newCurrent then newCurrent else prevTop
    bottom = if LZ.endp newCurrent then newCurrent else prevBottom
  in
    InfiniteListZipper { top, bottom, current: newCurrent }