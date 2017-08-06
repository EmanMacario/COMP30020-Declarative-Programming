-- Week 2 Workshop: 
-- Question 5 - XOR function implementation
-- Date: 1/08/17
-- Author: Emmanuel Macario <macarioe>

-- My implementation, using logical operations
xor :: Bool -> Bool -> Bool
xor a b = (not a && b) || (a && not b)

-- Using comparison
xor' :: Bool -> Bool -> Bool
xor' x y = x /= y

-- Using pattern matching
xor1 :: Bool -> Bool -> Bool
xor1 True True = False
xor1 False False = False
xor1 _ _ = True