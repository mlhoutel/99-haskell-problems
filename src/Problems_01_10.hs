module Problems_01_10 where

-- Nested list (needed from 7.)
data NestedList a = Elem a | List [NestedList a]

toString :: Show a => (NestedList a) -> String
toString (Elem x) = show x 
toString (List []) = "[]"
toString (List ls) = "[" ++ (parseList ls)
    where   parseList :: Show a => [NestedList a] -> String
            parseList [] = ""
            parseList [x] = toString x ++ "]"
            parseList (x:xs) = (toString x) ++ ", " ++ (parseList xs)

-- 1. Find the last element of a list. 
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

-- 2. Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "Empty list"
myButLast [_] = error "One element list"
myButLast (x:[l]) = x
myButLast (x:xs) = myButLast xs

-- 3. Find the K'th element of a list. The first element in the list is number 1.  
elementAt :: [a] -> Int -> a
elementAt [] n = error "Out of bounds"
elementAt (x:_) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)

-- 4. Find the number of elements of a list.  
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = (myLength xs) + 1

-- 5. Reverse a list.   
myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]
myReverse [] = []

-- 6. Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome x = x == (myReverse x)

-- 7. Flatten a nested list structure.
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List (x:xs)) = (myFlatten x) ++ (myFlatten (List xs))
myFlatten (List []) = []

-- 8. Eliminate consecutive duplicates of list elements.
myCompress :: Eq a => [a] -> [a]
myCompress [] = []
myCompress [x] = [x]
myCompress (x1:x2:xs)
    | x1 == x2 = myCompress([x2] ++ xs)
    | otherwise = [x1] ++ myCompress([x2] ++ xs)

-- 9. Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.
myPack :: (Eq a) => [a] -> [[a]]
myPack = foldr myPack' []
    where myPack' x []     = [[x]]
          myPack' x (y:xs)
            | x == (head y) = ((x:y):xs)
            | otherwise = ([x]:y:xs)

-- 10. Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.
myEncode :: Eq a => [a] -> [(a, Int)]
myEncode ls = map (\x -> (head x, length x)) (myPack ls)