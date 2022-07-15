module Tests_01_10 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Problems_01_10

suite_01_10 :: TestTree
suite_01_10 =
  testGroup
    "Problems_01_10"
    [ problem1 
      , problem2
      , problem3
      , problem4
      , problem5
    ]

-- ================== Problem 1 suite ================== 
testLastInt = testCase "Last of list with Ints" $
              assertEqual [] 4 (myLast [1,2,3,4])

testLastChar = testCase "Last of list with Chars" $
                assertEqual [] 'z' (myLast ['x','y','z'])

problem1 :: TestTree
problem1 = testGroup "Problem 1 suite" [testLastInt, testLastChar]

-- ================== Problem 2 suite ================== 
testButLastInt = testCase "Last but one of list with Ints" $
              assertEqual [] 3 (myButLast [1,2,3,4])

testButLastChar = testCase "Last  but one of list with Chars" $
                assertEqual [] 'y' (myButLast ['a'..'z'])

problem2 :: TestTree
problem2 = testGroup "Problem 2 suite" [testButLastInt, testButLastChar]

-- ================== Problem 3 suite ================== 
testElementAtInt = testCase "Element at with list of Ints" $
              assertEqual [] 2 (elementAt [1,2,3] 2)

testElementAtChar = testCase "Element at with list of Chars" $
                assertEqual [] 'e' (elementAt "haskell" 5)

problem3 :: TestTree
problem3 = testGroup "Problem 3 suite" [testElementAtInt, testElementAtChar]

-- ================== Problem 4 suite ================== 
testLengthInt = testCase "Lenght with list of Ints" $
              assertEqual [] 3 (myLength [123, 456, 789])

testLengthChar = testCase "Lenght with list of Chars" $
                assertEqual [] 13 (myLength "Hello, world!")

problem4 :: TestTree
problem4 = testGroup "Problem 4 suite" [testLengthInt, testLengthChar]

-- ================== Problem 5 suite ================== 
testReverseInt = testCase "Reverse list of Ints" $
              assertEqual [] [4,3,2,1] (myReverse [1,2,3,4])

testReverseChar = testCase "Reverse list of Chars" $
                assertEqual [] "!amanap ,lanac a ,nalp a ,nam A" (myReverse "A man, a plan, a canal, panama!")

problem5 :: TestTree
problem5 = testGroup "Problem 5 suite" [testReverseInt, testReverseChar]
