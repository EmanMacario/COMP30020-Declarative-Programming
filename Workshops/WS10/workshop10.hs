-- COMP30020: Week 11 Workshop 10 Question Solutions
-- Date: 10/10/17
-- Author: Emmanuel Macario <macarioe>

module Main where

import Data.Char (isDigit, digitToInt)
import System.IO (hFlush, stdout)


-- Aside: Monads
dealWithIt :: Nullable a -> (a -> Nullable b) -> Nullable b
dealWithIt v f = case v of
                    Null -> Null
                    Some x -> Some $ f x




-- Question 1
maybe_tail :: [a] -> Maybe [a]
maybe_tail [] = Nothing
maybe_tail (_:xs) = Just xs


maybe_drop :: Int -> [a] -> Maybe [a]
maybe_drop 0 xs = Just xs
maybe_drop n xs
    | n > 0 = maybe_tail xs >>= maybe_drop (n-1)


maybe_drop' :: Int -> [a] -> Maybe [a]
maybe_drop' 0 xs = Just xs
maybe_drop' n xs 
    | n > 0 = let mt = maybe_tail xs in 
              case mt of
                  Nothing  -> Nothing
                  Just xs' -> maybe_drop (n-1) xs'



-- Question 2
data Tree a = Empty | Node (Tree a) a (Tree a)

print_tree :: Show a => Tree a -> IO ()
print_tree Empty = putStr ""
print_tree (Node l v r) = do
    putStrLn $ show v
    print_tree l
    print_tree r


x = (Node (Node Empty 1 Empty) 2 (Node Empty 3 Empty))


-- Question 3
{-
str_to_num :: String -> Maybe Int
str_to_num (c:cs) = 
    | isDigit c = putStrLn $ digitToInt c >>= \_ -> str_to_num cs
    | otherwise = Nothing


all_digits [] = True
all_digits (c:cs) = isDigit c && all_digits cs
-}



-- Question 4




-- Question 5



-- Aside: List comprehension is syntactic sugar for a do statement
possiblePairs :: [a] -> [b] -> [(a,b)]
possiblePairs xs ys  = do
    x <- xs
    y <- ys
    return (x,y)


-- Read 'A Fistful of Monads' from LYAHS