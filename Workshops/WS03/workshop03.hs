-- COMP30020: Week 4 Workshop Question Solutions
-- Date: 14/08/17
-- Author: Emmanuel Macario <macarioe>

-- QUESTION 1
-- I would have the web server program generate the output in the form of a 
-- representation such as the HTML type of the previous question, and then 
-- convert that to a string and then print the string. This is so we cannot
-- have tags and attributes that violate syntax, since each tag or attribute
-- has to be an instance of a particular discriminated union type.


-- QUESTION 2
-- Converts a temperature in degrees Fahrenheit to degrees Celsius
ftoc :: Double -> Double
ftoc f = (5 / 9) * (f - 32)


-- QUESTION 3
-- Computes the roots of the quadratic equation defined by
-- 0 = a*x^2 + b*x + c, given a, b, and c
quadRoots :: Double -> Double -> Double -> [Double]
quadRoots a b c = [(-b + sqrt (b^2 - 4*a*c))/(2*a), 
                        (-b - sqrt (b^2 - 4*a*c))/(2*a)]


-- QUESTION 4
-- Merges two sorted lists into a single sorted list
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge ys [] = ys
merge (x:xs) (y:ys) 
    | x < y     = x : merge xs (y:ys)
    | otherwise = y : merge (x:xs) ys


-- QUESTION 5
-- Quicksort algorithm for a single list. Since Haskell is a purely functional
-- language, it is impossible to generate a truely random pivot for each
-- iteration of partitioning. This is because in a purely functional language,
-- the same input in a function call always produces the same output
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (pivot:xs) = quicksort lt ++ [pivot] ++ quicksort gtet
    where lt   = filter (< pivot) xs
          gtet = filter (>=pivot) xs



-- QUESTION 6
-- BST Type Declaration
data Tree k v = Leaf | Node k v (Tree k v) (Tree k v)
        deriving (Eq, Show)

-- Returns True if the two trees have the same shape: same arrangement
-- of nodes and leaves, but possibly different keys and values in the nodes.
same_shape :: Tree a b -> Tree c d -> Bool
same_shape Leaf Leaf           = True
same_shape Leaf (Node _ _ _ _) = False
same_shape (Node _ _ _ _) Leaf = False
same_shape (Node _ _ t1 t2) (Node _ _ t3 t4) = same_shape t1 t3 
                                                && same_shape t2 t4 


-- QUESTION 7
-- The following type definitions allow us to represent
-- expressions containing integers, variables "a" and "b", and operators
-- for addition, subtraction, multiplication and division
data Variable = A | B
     deriving (Show)

data Expression
       = Var Variable
       | Num Integer
       | Plus Expression Expression
       | Minus Expression Expression
       | Times Expression Expression
       | Div Expression Expression
       deriving (Show)


-- Takes the values of a and b and an expression, and returns the
-- value of the expression. For example eval 3 4 exp1 = 10, where 
-- exp1 = Plus (Times (Num 2) (Var A)) (Var B) = 2*a + b
eval :: Integer -> Integer -> Expression -> Integer 
eval a b (Var A) = a
eval a b (Var B) = b
eval a b (Num n) = n
eval a b (Plus expr1 expr2) = (eval a b expr1) + (eval a b expr2) 
eval a b (Minus expr1 expr2) = (eval a b expr1) - (eval a b expr2)
eval a b (Times expr1 expr2) = (eval a b expr1) * (eval a b expr2)
eval a b (Div expr1 expr2) = quot (eval a b expr1) (eval a b expr2)


--------------------------------------------------------------------------------

-- Random shit
f_add :: Int -> Int -> Int
f_add x y = x + y
  where y = 5
         where x = 7