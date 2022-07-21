module Tests_21_28 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Problems_21_28

suite_21_28 :: TestTree
suite_21_28 =
  testGroup
    "Problems_21_28"
    [
      problem21
      , problem22
    ]

    
-- ================== Problem 21 suite ================== 
testInsertAtInts = testCase "Insert at index with list of Ints" $
              assertEqual [] [1,2,3,4] (myInsertAt 3 [1,2,4] 3) 

testInsertAtChars = testCase "Insert at index with list of Chars" $
                assertEqual [] "aXbcd" (myInsertAt 'X' "abcd" 2)

problem21:: TestTree
problem21 = testGroup "Problem 21 suite" [testInsertAtInts, testInsertAtChars]

-- ================== Problem 22 suite ================== 
testRangeInts = testCase "Create range with list of Ints" $
              assertEqual [] [3,4,5,6,7,8,9] (myRange 3 9) 

testRangeChars = testCase "Create range with one elem" $
                assertEqual [] [1] (myRange 1 1)

problem22:: TestTree
problem22 = testGroup "Problem 22 suite" [testRangeInts, testRangeChars]
