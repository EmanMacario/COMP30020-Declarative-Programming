-- Week 2 Workshop: Function that appends two lists together
-- Date: 1/08/17
-- Author: Emmanuel Macario <macarioe>

-- Function type declaration
appendTwoLists :: [t] -> [t] -> [t]
appendTwoLists (x:xs) (y:ys) = x:(y:ys)

appendTwoLists
appendTwoLists [] [] = []