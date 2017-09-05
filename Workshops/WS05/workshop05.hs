-- COMP30020: Week 6 Workshop Question Solutions
-- Date: 30/08/17
-- Author: Emmanuel Macario <macarioe>


----
-- SOLUTIONS
----


-- QUESTION 1
-- Yields Nothing when the input Maybe is Nothing, and
-- applies the supplied function to the content of the Maybe when it is
-- Just some content.
maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply f Nothing = Nothing
maybeApply f (Just x) = Just (f x) 


-- QUESTION 2
-- Constructs a list of the result of applying the first argument to
-- corresponding elements of the two input lists.  If the two list
-- arguments are different lengths, the extra elements of the longer one
-- are ignored.
zWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zWith f [] _ = []
zWith f _ [] = []
zWith f (x:xs) (y:ys) = f x y : zWith f xs ys



-- QUESTION 3
-- Constructs a list of the result of multiplying each element in the 
-- third argument by the first argument, and then adding the second argument.
linearEqn :: Num a => a -> a -> [a] -> [a]
linearEqn _ _ [] = []
linearEqn x y xs = map ((+y) . (*x)) xs



-- QUESTION 4
-- Using function sqrtPM, define a function allSqrts that takes a list and
-- returns a list of all the positive and negative square roots of all
-- the numbers on the list. For example:
sqrtPM :: (Floating a, Ord a) => a -> [a]
sqrtPM x
    | x  > 0    = let y = sqrt x in [y, -y]
    | x == 0    = [0]
    | otherwise = []


allSqrts :: (Floating a, Ord a) => [a] -> [a]
allSqrts [] = []
allSqrts xs = foldr (++) [] $ map sqrtPM xs


{-
QUESTION 5
Lectures have given the definitions of two higher order functions in the
Haskell prelude, filter and map:

    filter :: (a -> Bool) -> [a] -> [a]
    map :: (a -> b) -> [a] -> [b]

Filter returns those elements of its argument list for which the given function
returns True, while map applies the given function to every element of the
given list.

Suppose you have a list of numbers, and you want to (a) filter out all the
negative numbers, and (b) apply the sqrt function to all the remaining
integers.

(a) Write code to accomplish this task using filter and map.
(b) Write code to accomplish this task that does only one list traversal,
without any higher order functions.
(c) Transform (b) to (a).
-}


filterNegsSqrtAll :: (Floating a, Ord a) => [a] -> [a]
filterNegsSqrtAll [] = []
filterNegsSqrtAll xs = map sqrt $ filter (\x -> x > 0) xs


filterNegsSqrtAll' :: (Floating a, Ord a) => [a] -> [a]
filterNegsSqrtAll' [] = []
filterNegsSqrtAll' (x:xs) = if x > 0
                            then 
                                sqrt x : filterNegsSqrtAll' xs
                            else 
                                filterNegsSqrtAll' xs 