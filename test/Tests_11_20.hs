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
      , problem13
      , problem14
      , problem15
      , problem16
      , problem17
      , problem18
      , problem19
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

-- ================== Problem 13 suite ================== 
testDirectEncodeInts = testCase "Direct encode Ints to SimpleTuple" $
              assertEqual [] "[(4, 1), 2, (2, 3), (2, 1), 4, (4, 5)]" (toString (myEncodeDirect [1,1,1,1,2,3,3,1,1,4,5,5,5,5]))

testDirectEncodeChars = testCase "Direct encode Chars to SimpleTuple" $
                assertEqual [] "[(4, 'a'), 'b', (2, 'c'), (2, 'a'), 'd', (4, 'e')]" (toString (myEncodeDirect "aaaabccaadeeee"))

problem13:: TestTree
problem13 = testGroup "Problem 13 suite" [testDirectEncodeInts, testDirectEncodeChars]

-- ================== Problem 14 suite ================== 
testDuplicateInts = testCase "Duplicate with list of Ints" $
              assertEqual [] [1,1,2,2,3,3] (myDuplicate [1,2,3])

testDuplicateChars = testCase "Duplicate with list of Chars" $
                assertEqual [] "aabbccdd" (myDuplicate "abcd")

problem14:: TestTree
problem14 = testGroup "Problem 14 suite" [testDuplicateInts, testDuplicateChars]

-- ================== Problem 15 suite ================== 
testReplicateInts = testCase "Replicate with list of Ints" $
              assertEqual [] [1,1,1,2,2,2,3,3,3] (myReplicate [1,2,3] 3)

testReplicateChars = testCase "Replicate with list of Chars" $
                assertEqual [] "aaaaabbbbb" (myReplicate "ab" 5)

problem15:: TestTree
problem15 = testGroup "Problem 15 suite" [testReplicateInts, testReplicateChars]

-- ================== Problem 16 suite ================== 
testDropInts = testCase "Drop with list of Ints" $
              assertEqual [] [1,2,4,5,7,8] (myDrop [1,2,3,4,5,6,7,8,9] 3)

testDropChars = testCase "Drop with list of Chars" $
                assertEqual [] "abdeghk" (myDrop "abcdefghik" 3)

problem16:: TestTree
problem16 = testGroup "Problem 16 suite" [testDropInts, testDropChars]

-- ================== Problem 17 suite ================== 
testSplitInts = testCase "Split with list of Ints" $
                assertEqual [] ([1, 2, 3, 4, 5], [6, 7, 8, 9]) (mySplit [1, 2, 3, 4, 5, 6, 7, 8, 9] 5)

testSplitChars = testCase "Split with list of Chars" $
                assertEqual [] ("abc", "defghik") (mySplit "abcdefghik" 3)

problem17:: TestTree
problem17 = testGroup "Problem 17 suite" [testSplitInts, testSplitChars]

-- ================== Problem 18 suite ================== 
testSliceInts = testCase "Slice with list of Ints" $
                assertEqual [] [2, 3, 4, 5] (mySlice [1, 2, 3, 4, 5, 6, 7, 8, 9] 2 5)

testSliceChars = testCase "Slice with list of Chars" $
                assertEqual [] "cdefg" (mySlice "abcdefghik" 3 7)

problem18:: TestTree
problem18 = testGroup "Problem 18 suite" [testSliceInts, testSliceChars]

-- ================== Problem 19 suite ================== 
testRotateInts = testCase "Rotate with list of Ints" $
                assertEqual [] [3, 4, 5, 1, 2] (myRotate [1, 2, 3, 4, 5] 2)

testRotateChars = testCase "Rotate with list of Chars" $
                assertEqual [] "defghabc" (myRotate "abcdefgh" 3)

testRotateNegative = testCase "Rotate with Negative index" $
                assertEqual [] "ghabcdef" (myRotate "abcdefgh" (-3))

problem19:: TestTree
problem19 = testGroup "Problem 19 suite" [testRotateInts, testRotateChars, testRotateNegative]

-- ================== Problem 20 suite ================== 
testRemoveInts = testCase "Remove with list of Ints" $
                assertEqual [] [1, 3, 4, 5] (myRotate [1, 2, 3, 4, 5] 2)

testRemoveChars = testCase "Remove with list of Chars" $
                assertEqual [] "abdefgh" (myRotate "abcdefgh" 3)

problem20:: TestTree
problem20 = testGroup "Problem 20 suite" [testRotateInts, testRotateChars]