-- Week 2 Workshop: XOR function implementation
-- Date: 1/08/17
-- Author: Emmanuel Macario <macarioe>

xor :: Bool -> Bool -> Bool
xor a b = (not a && b) || (a && not b)