module Tests_01_10 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Problems_01_10

suite_01_10 :: TestTree
suite_01_10 =
  testGroup
    "Problems_01_10"
    [ problem1 ]

-- ================== Problem 1 suite ================== 
testLastInt = testCase "Last of list with Ints" $
              assertEqual [] (myLast [1,2,3,4]) 4

testLastChar = testCase "Last of list with Chars" $
                assertEqual [] (myLast ['x','y','z']) 'z'

problem1 :: TestTree
problem1 = testGroup "Problem 1 suite" [testLastInt, testLastChar]
