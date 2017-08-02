-- Week 2 Workshop: Question 6 - Function that appends two lists together
-- Date: 1/08/17
-- Author: Emmanuel Macario <macarioe>

-- Function type declaration
append :: [t] -> [t] -> [t]


-- Function definition
-- Base Case: Appending empty-list onto another (possibly empty) list
append []     ys = ys

-- Recursive Case: Append head of left list to head of right list
append (x:xs) ys = x : append xs ys