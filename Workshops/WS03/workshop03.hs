-- COMP30020: Week 4 Workshop Question Solutions
-- Date: 14/08/17
-- Author: Emmanuel Macario <macarioe>

-- QUESTION 1



-- QUESTION 2
-- Converts a temperature in degrees Fahrenheit to degrees Celsius
ftoc :: Double -> Double
ftoc f = (5 / 9) * (f - 32)


-- QUESTION 3
-- Computes the roots of the quadratic equation defined by
-- 0 = a*x^2 + b*x + c, given a, b, and c
quadRoots :: Double -> Double -> Double -> [Double]
quadRoots a b c = [(-b + sqrt (b^2 - 4*a*c))/(2*a), 
                        (-b - sqrt (b^2 - 4*a*c))/(2*a)]


-- QUESTION 4
-- Merges two sorted lists into a single sorted list
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge ys [] = ys
merge (x:xs) (y:ys) 
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


-- QUESTION 5
-- Quicksort algorithm for lists
-- quicksort :: [a] -> [a] -> [a]


-- QUESTION 6
-- BST Type Declaration
data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)
        deriving (Eq, Show)

-- Returns True if the two trees have the same shape: same arrangement
-- of nodes and leaves, but possibly different keys and values in the nodes.
same_shape :: Tree a b -> Tree c d -> Bool