module Tests_01_10 where

import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

import Problems_01_10

suite_01_10 :: TestTree
suite_01_10 =
  testGroup
    "Problems_01_10"
    [ nestedList
      , problem1 
      , problem2
      , problem3
      , problem4
      , problem5
      , problem6
      , problem7
      , problem8
      , problem9
      , problem10
    ]

-- ================== NestedList suite ================== 
testConvertEmpty = testCase "Convert empty to Nested" $
              assertEqual [] "[]" (toString (List [] :: NestedList ()))
              
testConvertList = testCase "Convert list of Ints to Nested" $
              assertEqual [] "[[1, 2], 3, 4]" (toString (List [(List [(Elem 1), (Elem 2)]), (Elem 3), (Elem 4)]))

testConvertChars = testCase "Convert list of Chars to Nested" $
              assertEqual [] "[['a', 'b'], 'c', 'd']" (toString (List [(List [(Elem 'a'), (Elem 'b')]), (Elem 'c'), (Elem 'd')]))

nestedList :: TestTree
nestedList = testGroup "NestedList suite" [testConvertEmpty, testConvertList, testConvertChars]

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

-- ================== Problem 6 suite ================== 
testPalindromeFalse = testCase "Palindrome false" $
              assertEqual [] False (isPalindrome [1,2,3])

testPalindromeInt = testCase "Palindrome with list of Ints" $
              assertEqual [] True (isPalindrome [1,2,4,8,16,8,4,2,1])

testPalindromeChar = testCase "Palindrome with list of Chars" $
                assertEqual [] True (isPalindrome "madamimadam")

problem6 :: TestTree
problem6 = testGroup "Problem 6 suite" [testPalindromeFalse, testPalindromeInt, testPalindromeChar]

-- ================== Problem 7 suite ================== 
testFlattenElem = testCase "Flatten Elem with Int" $
              assertEqual [] [5] (myFlatten (Elem 5))

testFlattenInt = testCase "Flatten with list of Ints" $
              assertEqual [] [1,2,3,4,5] (myFlatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]))

testFlattenChar = testCase "Flatten empty" $
                assertEqual [] [] (myFlatten (List [] :: NestedList ()))

problem7 :: TestTree
problem7 = testGroup "Problem 7 suite" [testFlattenElem, testFlattenInt, testFlattenChar]

-- ================== Problem 8 suite ================== 
testCompressInt = testCase "Compress consecutive with list of Ints" $
              assertEqual [] [2, 4, 6, 8, 6, 5] (myCompress [2, 2, 2, 4, 6, 6, 8, 6, 5])

testCompressChars = testCase "Compress consecutive with list of Chars"  $
              assertEqual [] "abcade" (myCompress  "aaaabccaadeeee")

problem8 :: TestTree
problem8 = testGroup "Problem 8 suite" [testCompressInt, testCompressChars]

-- ================== Problem 9 suite ================== 
testPackInts = testCase "Pack consecutive with list of Ints" $
              assertEqual [] [[2, 2, 2], [4], [6, 6], [8], [6], [5]] (myPack [2, 2, 2, 4, 6, 6, 8, 6, 5])

testPackChars = testCase "Pack consecutive with list of Chars"  $
              assertEqual [] ["aaaa", "b", "cc", "aa", "d", "eeee"] (myPack  "aaaabccaadeeee")

problem9 :: TestTree
problem9 = testGroup "Problem 9 suite" [testPackInts, testPackChars]

-- ================== Problem 10 suite ================== 
testEncodeInts = testCase "Length encode with list of Ints" $
              assertEqual [] [(2, 3), (4, 1), (6, 2), (8, 1), (6, 1), (5, 1)] (myEncode [2, 2, 2, 4, 6, 6, 8, 6, 5])

testEncodeChars = testCase "Length encode with list of Chars"  $
              assertEqual [] [('a', 4), ('b', 1), ('c', 2), ('a', 2), ('d', 1), ('e', 4)] (myEncode "aaaabccaadeeee")

problem10 :: TestTree
problem10 = testGroup "Problem 10 suite" [testEncodeInts, testEncodeChars]
