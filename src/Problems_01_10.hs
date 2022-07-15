module Problems_01_10 where

-- 1. Find the last element of a list. 
myLast :: [a] -> a
myLast [] = error "Empty list"
myLast [a] = a
myLast (x:xs) = myLast xs