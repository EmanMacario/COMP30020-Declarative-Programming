-- Real World Haskell - Chapter 2: Types and Functions

-- Example implementation of standard prelude function 'drop'
myDrop :: Int -> [a] -> [a]
myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n-1) (tail xs)

-- Side note: the expressions that follow the 'then' and 'else' keywords
-- must have same type. 