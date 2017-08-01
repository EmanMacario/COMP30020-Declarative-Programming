-- Program that calculates and returns the length of a list

len :: [t] -> Int
len [] = 0
len (x:xs) = 1 + len xs