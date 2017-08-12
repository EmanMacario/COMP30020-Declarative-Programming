-- Week 3 Lecture 2 Quiz: 
-- Write a function that filters negative numbers from a list of numbers
-- Author: Emmanuel Macario
-- Date: 12/08/17

filterNegative :: (Num a) => (Ord a) => [a] -> [a]
filterNegative []     = []
filterNegative (x:xs) = if x < 0
                        then
                            filterNegative xs
                        else x : filterNegative xs 

-- Aside: note that the list operator (:) is right associative. This is why
-- the expression given in the 'else' clause works. For more information, type
-- :info (:) in ghci 
-- 
-- E.g. > 2:3:4:[5,6,7,8]
--        [2,3,4,5,6,7,8]
