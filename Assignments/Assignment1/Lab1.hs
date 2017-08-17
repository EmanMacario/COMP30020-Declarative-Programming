-- COMP30020 Declarative Programming Assignment 1
-- File: Lab1.hs
-- Author: <Emmanuel Macario> <macarioe>
-- Student Number: 831659
-- Email: macarioe@student.unimelb.edu.au
-- Origin: Thurs Aug 17 09:36:00 2017
-- Purpose: Contains a small module of simple Haskell functions


module Lab1 (subst, interleave, unroll) where


-- 'subst' takes two values and a list, and replaces every occurrence of 
-- the first value with the second value.
subst :: Eq t => t -> t -> [t] -> [t]
subst _ _ [] = []
subst a b xs = if head xs == a
               then
                   b : subst a b (tail xs)
               else
                   head xs : subst a b (tail xs)



-- 'interleave' takes two lists and returns the interleaving of two arguments.
-- That, the result is a list in which every odd element comes from the first
-- list, and every even element comes from the second. If lists are of unequal
-- length, the excess elements from the longer list comprise of the end of
-- the resulting list.
interleave :: [t] -> [t] -> [t]
interleave xs [] = xs
interleave [] xs = xs
interleave xs ys = head xs : interleave ys (tail xs) 



-- 'unroll' takes a list and an integer and creates a list of specified length,
-- by repeating the elements of the input list as many times as necessary to
-- achieve the specified length. If the integer is positive and an empty list 
-- is given, it will return an empty list.
unroll :: Int -> [a] -> [a]
unroll 0 _  = []
unroll _ [] = []
unroll n (x:xs) = x : unroll (n-1) (xs ++ [x])


