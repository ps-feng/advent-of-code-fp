module Test.ListZipper where

import Day9.ListZipper
import Prelude
import Test.Assert

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Partial.Unsafe (unsafePartial)

main :: Effect Unit
main = do
  testBeginpOnEmptyZipper
  testBeginpOnSingletonZipper
  testEndpOnEmptyZipper
  testEndpOnSingletonZipper
  testCursorOnEmptyZipper
  testCursorOnSingletonZipper
  testSafeCursorOnEmptyZipper
  testSafeCursorOnSingletonZipper
  testRightOnEmptyZipper
  testRightOnSingletonZipper
  testLeftOnEmptyZipper
  testInsertOnEmptyZipper
  testDeleteOnEmptyZipper
  testDeleteOnSingletonZipper
  testReversezOnEmptyZipper
  testReversezOnSingletonZipper
  testReversez

testBeginpOnEmptyZipper :: Effect Unit
testBeginpOnEmptyZipper = 
  assertTrue $ beginp empty

testBeginpOnSingletonZipper :: Effect Unit
testBeginpOnSingletonZipper =
  assertTrue (beginp $ singleton 10)

testEndpOnEmptyZipper :: Effect Unit
testEndpOnEmptyZipper = 
  assertTrue $ endp empty

testEndpOnSingletonZipper :: Effect Unit
testEndpOnSingletonZipper =
  assertFalse (endp $ singleton 10)

testSafeCursorOnEmptyZipper :: Effect Unit
testSafeCursorOnEmptyZipper =
  assertEqual 
    { actual: safeCursor (empty :: ListZipper Int)
    , expected: Nothing
    }

testSafeCursorOnSingletonZipper :: Effect Unit
testSafeCursorOnSingletonZipper =
  assertEqual 
    { actual: safeCursor $ singleton 10
    , expected: Just 10
    }

testCursorOnEmptyZipper :: Effect Unit
testCursorOnEmptyZipper =
  assertThrows \_ -> unsafePartial $ cursor (empty :: ListZipper Int)

testCursorOnSingletonZipper :: Effect Unit
testCursorOnSingletonZipper =
  assertEqual 
    { actual: unsafePartial $ cursor $ singleton 10
    , expected: 10
    }

testRightOnEmptyZipper :: Effect Unit
testRightOnEmptyZipper =
  assertEqual 
    { actual: right (empty :: ListZipper Int)
    , expected: Nothing
    }

testRightOnSingletonZipper :: Effect Unit
testRightOnSingletonZipper =
  assertEqual 
    { actual: safeCursor =<< (right $ singleton 10)
    , expected: Nothing
    }

testLeftOnEmptyZipper :: Effect Unit
testLeftOnEmptyZipper =
  assertEqual 
    { actual: left (empty :: ListZipper Int)
    , expected: Nothing
    }

testInsertOnEmptyZipper :: Effect Unit
testInsertOnEmptyZipper =
  assertEqual 
    { actual: insert 10 (empty :: ListZipper Int)
    , expected: singleton 10
    }

testDeleteOnEmptyZipper :: Effect Unit
testDeleteOnEmptyZipper =
  assertEqual 
    { actual: delete (empty :: ListZipper Int)
    , expected: empty
    }

testDeleteOnSingletonZipper :: Effect Unit
testDeleteOnSingletonZipper =
  assertEqual 
    { actual: delete $ singleton 10
    , expected: empty
    }

testReversezOnEmptyZipper :: Effect Unit
testReversezOnEmptyZipper =
  assertEqual 
    { actual: reversez (empty :: ListZipper Int)
    , expected: empty
    }

testReversezOnSingletonZipper :: Effect Unit
testReversezOnSingletonZipper =
  assertEqual 
    { actual: reversez $ singleton 10
    , expected: singleton 10
    }

testReversez :: Effect Unit
testReversez =
  assertEqual
    { actual: reversez $ fromFoldable [0, 10, 20]
    , expected: fromFoldable [20, 10, 0]
    }