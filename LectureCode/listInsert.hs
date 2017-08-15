-- Week 4 lecture 1: Quiz
-- Date: 15/08/17
listInsert :: Int -> [Int] -> [Int]
listInsert n [] = [n]
listInsert n (x:xs) 
    | n > x     = x:listInsert n xs
    | otherwise = n:x:xs