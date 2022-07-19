module Problems_11_20 where

-- Nested list (needed from 11.)
data SimpleTuple a = Simple a | Multiple Int a

toString :: Show a => [(SimpleTuple a)] -> String
toString [Simple x] = show x 
toString [Multiple i x] = "(" ++ (show i) ++ ", " ++ (show x) ++ ")"
toString [] = "[]"
toString ls = "[" ++ (parseList ls)
    where   parseList :: Show a => [(SimpleTuple a)] -> String
            parseList [] = ""
            parseList [x] = toString [x] ++ "]"
            parseList (x:xs) = (toString [x]) ++ ", " ++ (parseList xs)

-- 11. Modified run-length encoding. Only elements with duplicates are transferred as (N E) lists.
myEncodeModified :: [(a, Int)] -> [(SimpleTuple a)]
myEncodeModified [] = []
myEncodeModified ((x, 1):xs) = [(Simple x)] ++ (myEncodeModified xs)
myEncodeModified ((x, n):xs) = [(Multiple n x)] ++ (myEncodeModified xs)

-- 12. Decode a run-length encoded list. Construct its uncompressed version. 
myDecodeModified :: [(SimpleTuple a)] -> [a]
myDecodeModified [] = []
myDecodeModified ((Simple x):xs) = [x] ++ (myDecodeModified xs)
myDecodeModified ((Multiple n x):xs) = (addWhile n x) ++ (myDecodeModified xs)
    where   addWhile :: Int -> a -> [a]
            addWhile 0 x = []
            addWhile n x = [x] ++ (addWhile (n-1) x)

-- 13. Run-length encoding of a list (direct solution => don't explicitly create the sublists containing the duplicates).
myEncodeDirect :: Eq a => [a] -> [(SimpleTuple a)]
myEncodeDirect [] = [] 
myEncodeDirect (x:xs) = myEncodeDirect' xs (Simple x)
    where   myEncodeDirect' :: Eq a => [a] -> (SimpleTuple a) -> [(SimpleTuple a)]
            myEncodeDirect' [] s = [s]
            myEncodeDirect' (x:xs) (Simple a)
                | x == a = myEncodeDirect' xs (Multiple 2 x)
                | otherwise = [(Simple a)] ++ (myEncodeDirect' xs (Simple x))
            myEncodeDirect' (x:xs) (Multiple n a)
                | x == a = myEncodeDirect' xs (Multiple (n+1) x)
                | otherwise = [(Multiple n a)] ++ (myEncodeDirect' xs (Simple x))

-- 14. Duplicate the elements of a list.
myDuplicate :: [a] -> [a]
myDuplicate [] = []
myDuplicate (x:xs) = x:x:(myDuplicate xs)

-- 15. Replicate the elements of a list a given number of times.
myReplicate :: [a] -> Int -> [a]
myReplicate [] _ = []
myReplicate (x:xs) n = (addWhile n x) ++ (myReplicate xs n)
    where   addWhile :: Int -> a -> [a]
            addWhile 0 x = []
            addWhile n x = [x] ++ (addWhile (n-1) x)

-- 16. Drop every N'th element from a list.
myDrop :: [a] -> Int -> [a]
myDrop ls n = myDrop' ls n
    where   myDrop' :: [a] -> Int -> [a]
            myDrop' [] _ = []
            myDrop' (x:xs) 1 = myDrop' xs n
            myDrop' (x:xs) c = x : (myDrop' xs (c-1))

-- 17. Split a list into two parts; the length of the first part is given.
mySplit :: [a] -> Int -> ([a], [a])
mySplit ls n = mySplit' ls [] n
    where   mySplit' :: [a] -> [a] -> Int -> ([a], [a])
            mySplit' [] lx _ = (lx, [])
            mySplit' ls lx 0 = (lx, ls)
            mySplit' (x:xs) lx c = (mySplit' xs (lx ++ [x]) (c - 1))

-- 18. Extract a slice from a list..
mySlice :: [a] -> Int -> Int -> [a]
mySlice ls s e = snd (mySplit (fst (mySplit ls e)) (s - 1))

-- 19. Rotate a list N places to the left.
myRotate :: [a] -> Int -> [a]
myRotate ls n
    | n > 0 = (snd lx) ++ (fst lx)
    | n < 0 = (snd ly) ++ (fst ly)
    | otherwise = ls
    where
        lx = mySplit ls n
        ly = mySplit ls ((length ls) + (n + 1))

-- 20. Remove the K'th element from a list
myRemove :: [a] -> Int -> [a]
myRemove [] _ = []
myRemove (x:xs) 1 = xs
myRemove (x:xs) c = x : (myRemove xs (c - 1))