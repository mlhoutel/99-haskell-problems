module Problems_21_28 where

import System.Random

-- 21. Insert an element at a given position into a list.
myInsertAt :: a -> [a] -> Int ->  [a]
myInsertAt a [] _ = error "Empty list"
myInsertAt a xs 1 = a:xs
myInsertAt a (x:xs) n = x:(myInsertAt a xs (n-1))

-- 22. Create a list containing all integers within a given range.
myRange :: Int -> Int -> [Int]
myRange s e
    | s == e = [e]
    | otherwise = s:(myRange (s+1) e)
    
-- 23. Extract a given number of randomly selected elements from a list.
myRandomSelect :: Int -> [a] -> [a]
myRandomSelect n x = x -- TODO

-- 24. Lotto: Draw N different random numbers from the set 1..M.

-- 25. Generate a random permutation of the elements of a list.

-- 26. Generate the combinations of K distinct objects chosen from the N elements of a list

-- 27. Group the elements of a set into disjoint subsets.

-- 28. Sorting a list of lists according to length of sublists