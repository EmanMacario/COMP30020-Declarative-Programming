-- COMP30020: Week 11 Workshop 10 Question Solutions
-- Date: 10/10/17
-- Author: Emmanuel Macario <macarioe>

module Main where

import Data.Char (isDigit, digitToInt)
import System.IO (hFlush, stdout)


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
str_to_num :: String -> Maybe Int
str_to_num (c:cs) = 