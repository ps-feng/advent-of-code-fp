module Day9.ListZipper 
  ( ListZipper
  , fromFoldable
  , toList
  , empty
  , singleton
  , beginp
  , endp
  , cursor
  , safeCursor
  , right
  , left
  , insert
  , delete
  , reversez
  )
where

import Prelude

import Data.Foldable (class Foldable)
import Data.List ((:))
import Data.List as L
import Data.List.Partial (head)
import Data.Maybe (Maybe)

data ListZipper a = Zip (L.List a) (L.List a)

instance eqListZipper :: Eq a => Eq (ListZipper a) where
  eq lhs rhs = toList lhs == toList rhs

instance showListZipper :: Show a => Show (ListZipper a) where
  show (Zip parents list) = "Zip [" <> show parents <> "] [" <> show list <> "]"

fromFoldable :: forall f. Foldable f => f ~> ListZipper
fromFoldable = Zip L.Nil <<< L.fromFoldable

toList :: ListZipper ~> L.List
toList (Zip ls rs) = L.foldl (flip (:)) rs ls

empty :: forall a. ListZipper a
empty = Zip L.Nil L.Nil

singleton :: forall a. a -> ListZipper a
singleton a = Zip L.Nil (L.singleton a)

beginp :: forall a. ListZipper a -> Boolean
beginp (Zip L.Nil _) = true
beginp _ = false

endp :: forall a. ListZipper a -> Boolean
endp (Zip _ L.Nil) = true
endp _ = false

cursor :: forall a. Partial => ListZipper a -> a
cursor (Zip _ rs) = head rs

safeCursor :: forall a. ListZipper a -> Maybe a
safeCursor (Zip _ rs) = L.head rs

right :: forall a. ListZipper a -> Maybe (ListZipper a)
right (Zip ls rs) = do
    rhead <- L.head rs
    rtail <- L.tail rs
    pure $ Zip (rhead : ls) rtail

left :: forall a. ListZipper a -> Maybe (ListZipper a)
left (Zip ls rs) = do
  lhead <- L.head ls
  ltail <- L.tail ls
  pure $ Zip ltail (lhead : rs)

insert :: forall a. a -> ListZipper a -> ListZipper a
insert a (Zip ls rs) = Zip ls (a : rs)

delete :: forall a. ListZipper a -> ListZipper a
delete zipper@(Zip _ L.Nil) = zipper
delete (Zip ls (L.Cons r rs)) = Zip ls rs

reversez :: forall a. ListZipper a -> ListZipper a
reversez (Zip ls L.Nil) = Zip L.Nil ls
reversez (Zip ls (L.Cons r rs)) = Zip rs (r : ls)
