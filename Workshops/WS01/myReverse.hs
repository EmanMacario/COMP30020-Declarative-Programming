-- Week 2 Workshop: 
-- Question 7 - Own implementation of the standard 'reverse' function.
-- Date: 2/08/17
-- Author: Emmanuel Macario <macarioe>

-- Function type declaration
myReverse :: [t] -> [t]

-- Function definition
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]