module Problems_01_10 where

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