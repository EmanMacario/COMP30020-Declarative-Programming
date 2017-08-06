-- Real World Haskell - Chapter 2: Types and Functions

-- Returns the element before the last
lastButOne :: [a] -> a
lastButOne xs = if null (tail (tail xs))
                then
                    head xs
                else
                    lastButOne (tail xs)