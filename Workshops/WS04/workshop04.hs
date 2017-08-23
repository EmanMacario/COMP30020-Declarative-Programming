-- COMP30020: Week 5 Workshop Question Solutions
-- Date: 20/08/17
-- Author: Emmanuel Macario <macarioe>


----
-- SOLUTIONS
----

----
-- QUESTION 1: Tree sort algorithm
----

data Tree a = Leaf | Node (Tree a) a (Tree a)

-- Function for inserting values into a binary search tree. If an insertion 
-- value already exists in the tree, then insert the insertion value into
-- the right subtree of the equivalent value.
insert_bst :: Ord a => Tree a -> a -> Tree a
insert_bst Leaf iv = Node Leaf iv Leaf
insert_bst (Node l v r) iv =
    if iv >= v
    then 
        Node l v (insert_bst r iv)
    else
        Node (insert_bst l iv) v r


-- Retrieve elements from binary search tree in sorted (ascending) order
flatten :: Tree a -> [a]
flatten Leaf         = []
flatten (Node l v r) = flatten l ++ [v] ++ flatten r


-- Inserts all the to-be-sorted data items into a binary search tree, then
-- performs an in order traversal to extract the items in sorted order.
treesort :: Ord a => [a] -> [a]
treesort []     = []
treesort (x:xs) = 
    let
        assoc_list_to_bst []     = Leaf
        assoc_list_to_bst (y:ys) = insert_bst t0 y
            where t0 = assoc_list_to_bst ys
    in 
        flatten $ assoc_list_to_bst $ x:xs



----
-- QUESTION 2: Transposing a list of lists
----

transpose :: [[a]] -> [[a]]
transpose [] = error "transpose of zero-height matrix"
transpose list@(xs:xss)
  | len > 0   = transpose' len list
  | otherwise = error "transpose of zero-width matrix"
  where len = length xs

transpose' len [] = replicate len []
transpose' len (xs:xss)
  | len == length xs = zipWith (:) xs (transpose' len xss)
  | otherwise = error "transpose of non-rectangular matrix"



----
-- QUESTION 3: Parsing a number list
----

-- Functions take a list of numbers and returns a 3-tuple containing the
-- length, the sum of the numbers, and the sum of the squares of the numbers.

-- Many traversals version
trip :: Num a => [a] -> (Int, a, a)
trip xs = (length xs, sum xs, sum $ map (^2) xs)

-- One traversal version
trip' :: Num a => [a] -> (Int, a, a)
trip' xs = go xs (0, 0, 0)
    where
        go [] acc = acc
        go (x:xs) (c, s, sq) = go xs (c+1, x+s, sq+x^2)



--------------------------------------------------------------------------------

----
-- ASIDE:
----

-- '.' (dot operator) is used to tie the output of whatever appears on the
-- right, to be the input of whatever appears on the left.
-- E.g. 
-- > putStrLn . show $ 1 + 1
--   "2"


-- Operator sections
-- let g = (^4)
-- > g 2
--   16

-- Mapping
-- > map (+3) [7,5,10]
--   [10, 8, 13]