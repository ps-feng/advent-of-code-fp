module Test.InfiniteListZipper where

import Prelude

import Data.Maybe (Maybe(..))
import Day9.CircularList (current, remove, insert, left, right)
import Day9.ListZipper (ListZipper, empty, singleton)
import Effect (Effect)
import Test.Assert (assertEqual)

import Debug.Trace

main :: Effect Unit
main = do
  testCurrentOnEmptyList
  testCurrentOnSingletonList
  testRightOnSingletonList
  testLeftOnSingletonList
  testInsertOnEmptyList
  testInsertOnSingletonList
  testInsertOnSingletonListShouldAddElementAsLast
  testDeleteOnEmptyList
  testDeleteOnSingletonList
  testDeleteShouldRemoveCurrentElement

testCurrentOnEmptyList :: Effect Unit
testCurrentOnEmptyList = 
  assertEqual 
    { actual: current (empty :: ListZipper Int)
    , expected: Nothing
    }

testCurrentOnSingletonList :: Effect Unit
testCurrentOnSingletonList = 
  assertEqual 
    { actual: current $ singleton 10
    , expected: Just 10
    }

testRightOnSingletonList :: Effect Unit
testRightOnSingletonList =
  assertEqual 
    { actual: current $ right $ singleton 10
    , expected: Just 10
    }

testLeftOnSingletonList :: Effect Unit
testLeftOnSingletonList =
  assertEqual 
    { actual: current $ left $ singleton 10
    , expected: Just 10
    }

testInsertOnEmptyList :: Effect Unit
testInsertOnEmptyList =
  assertEqual 
    { actual: current $ insert 10 $ (empty :: ListZipper Int)
    , expected: Just 10
    }

testInsertOnSingletonList :: Effect Unit
testInsertOnSingletonList =
  assertEqual 
    { actual: current $ insert 20 $ singleton 10
    , expected: Just 20
    }

testInsertOnSingletonListShouldAddElementAsLast :: Effect Unit
testInsertOnSingletonListShouldAddElementAsLast =
  assertEqual 
    { actual: current $ right $ insert 20 $ singleton 10
    , expected: Just 10
    }

testDeleteOnEmptyList :: Effect Unit
testDeleteOnEmptyList =
  assertEqual 
    { actual: current $ remove (empty :: ListZipper Int)
    , expected: Nothing
    }

testDeleteOnSingletonList :: Effect Unit
testDeleteOnSingletonList = 
  assertEqual 
    { actual: current $ remove $ singleton 10
    , expected: Nothing
    }

testDeleteShouldRemoveCurrentElement :: Effect Unit
testDeleteShouldRemoveCurrentElement =
  assertEqual 
    { actual: current $ remove $ left $ insert 20 $ singleton 10
    , expected: Just 20
    }