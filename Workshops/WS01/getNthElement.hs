-- Week 2 Workshop: Question 8 
-- Implementation of function 'getNthElement', which takes an integer 'n' and
-- a list, and returns the nth element of the list (not using zero-indexing).

-- Date: 2/08/17
-- Author: Emmanuel Macario <macarioe>

getNthElement 1 (x:xs) = x
getNthElement n [] = error "'n' is greater than the length of the list"
getNthElement n (x:xs)
    | n < 1 = error "'n' must be a positive integer"
    | n > 1 = getNthElement (n-1) xs