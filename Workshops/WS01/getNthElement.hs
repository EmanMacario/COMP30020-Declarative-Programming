-- Week 2 Workshop: Question 8 
-- Implementation of function 'getNthElement', which takes an integer 'n' and
-- a list, and returns the nth element of the list (using zero-indexing).

-- Date: 2/08/17
-- Author: Emmanuel Macario <macarioe>

getNthElement :: Int -> [t] -> t
getNthElement 0 (x:xs) = x
getNthElement n (x:xs) = getNthElement (n-1) xs