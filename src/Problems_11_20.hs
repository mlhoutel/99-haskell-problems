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