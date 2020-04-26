module Test.FocusedArray where

import Prelude

import Data.Maybe (Maybe(..))
import Day9.CircularList (FocusedArray(..), current, remove, insert, left, right)
import Effect (Effect)
import Test.Assert (assertEqual)

singleton :: forall a. a -> FocusedArray a
singleton a = FocusedArray { currentIndex: 0, array: [a] }

main :: Effect Unit
main = do
   testCurrentOnSingletonList
   testRightOnSingletonList
   testLeftOnSingletonList
   testInsertOnSingletonList
   testInsertOnSingletonListShouldAddElementAsLast
   testDeleteOnSingletonList
   testDeleteShouldRemoveCurrentElement

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