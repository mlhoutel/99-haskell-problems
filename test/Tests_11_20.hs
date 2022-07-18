module Tests_11_20 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Problems_11_20

suite_11_20 :: TestTree
suite_11_20 =
  testGroup
    "Problems_11_20"
    [ simpleTuple
      , problem11
      , problem12
    ]

-- ================== SimpleTuple suite ================== 
testConvertEmpty = testCase "Convert empty to SimpleTuple" $
              assertEqual [] "[]" (toString ([] :: [SimpleTuple ()]))
              
testConvertList = testCase "Convert list of Ints to SimpleTuple" $
              assertEqual [] "[(2, 1), 3, 4]" (toString [Multiple 2 1, Simple 3, Simple 4])

testConvertChars = testCase "Convert list of Chars to Nested" $
              assertEqual [] "[(2, 'a'), 'b', 'c']" (toString [Multiple 2 'a', Simple 'b', Simple 'c'])

simpleTuple :: TestTree
simpleTuple = testGroup "SimpleTuple suite" [testConvertEmpty, testConvertList, testConvertChars]

-- ================== Problem 11 suite ================== 
testEncodeInts = testCase "Encode Ints to SimpleTuple" $
              assertEqual [] "[(3, 1), 4, (2, 6), 9]" (toString (myEncodeModified [(1, 3), (4, 1), (6, 2), (9, 1)]))

testEncodeChars = testCase "Encode Chars to SimpleTuple" $
                assertEqual [] "[(3, 'a'), 'b', (2, 'c'), 'd']" (toString (myEncodeModified [('a', 3), ('b', 1), ('c', 2), ('d', 1)]))

problem11:: TestTree
problem11 = testGroup "Problem 11 suite" [testEncodeInts, testEncodeChars]

-- ================== Problem 12 suite ================== 
testDecodeInts = testCase "Decode SimpleTuple to Ints" $
              assertEqual [] [1,1,1,1,2,3,3,1,1,4,5,5,5,5] (myDecodeModified [Multiple 4 1, Simple 2, Multiple 2 3, Multiple 2 1, Simple 4, Multiple 4 5])

testDecodeChars = testCase "Decode SimpleTuple to Chars" $
                assertEqual [] "aaaabccaadeeee" (myDecodeModified [Multiple 4 'a', Simple 'b', Multiple 2 'c', Multiple 2 'a', Simple 'd', Multiple 4 'e'])

problem12:: TestTree
problem12 = testGroup "Problem 12 suite" [testDecodeInts, testDecodeChars]