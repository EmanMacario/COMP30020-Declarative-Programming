-- COMP30020 Declarative Programming Assignment 1
-- Name: Emmanuel Macario
-- Student Number: 831659
-- Email: <macarioe@student.unimelb.edu.au>

module Lab1 (subst, interleave, unroll) where

-- QUESTION 1

{- 
subst takes two values and a list, and replaces every occurrence of the first value with
the second in the list. For example:
subst 0 1 [0,1,2,3] should return [1,1,2,3]
subst 'e' 'o' "dog" should return "dog"
subst 'e' 'o' "elephant" should return "olophant"
-}
subst :: Eq t => t -> t -> [t] -> [t]
subst a b [] = []
subst a b xs = if head xs == a
               then
                   [b] ++ subst a b (tail xs)
               else
                   [head xs] ++ subst a b (tail xs)


-- QUESTION 2
{-
2. interleave :: [t] -> [t] -> [t]
interleave takes two lists and returns the interleaving of the two arguments. That, the
result is a list in which the first, third, fifth . . . elements come from the first argument
and the second, fourth, sixth . . . come from second. If either argument is shorter than
the other, the excess elements of the longer comprise the end of the resulting list. For
example:

interleave [1,2,3,4] [11,12,13,14] should return [1,11,2,12,3,13,4,14]
interleave "" "dog" should return "dog"
interleave "wl" "arus" should return "walrus"
interleave "tlpone" "eeh" should return "telephone"

-}

interLeave :: [t] -> [t] -> [t]
interLeave xs [] = xs
interLeave [] xs = xs
interLeave xs ys = [head xs] ++ interLeave ys (tail xs) 


-- QUESTION 3
{- 
    unroll :: Int -> [a] -> [a]
    unroll takes a list and an integer and constructs a list of the specified length made up
    by \unrolling" the input list as many times as needed to construct a list of that length.
    That is, the output consists of the input list repeated as many times as necessary to
    have the specifed length. For example:

    unroll 3 [1,2,3,4,5] should return [1,2,3]
    unroll 8 [1,2,3] should return [1,2,3,1,2,3,1,2]
    unroll 4 "ski" should return "skis"
-}

unroll :: Int -> [a] -> [a]
unroll 0 xs = []
unroll n [] = []
unroll n xs = 


