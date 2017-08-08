-- Week 2 Workshop Question Solutions

-- QUESTION 2
data FontTag = FontTag (Maybe Int) (Maybe String) (Maybe FontColour)

data FontColour = ColorName String
                | Hex Int
                | RGB Int Int Int


-- QUESTION 3
factorial :: Int -> Int
factorial n
    | n < 0  = error "'n' must be a positive integer"
    | n == 0 = 1
    | n > 0  = n * factorial (n-1)


-- QUESTION 4
myElem :: Eq a => a -> [a] -> Bool
myElem elem []     = False
myElem elem (x:xs) = if x == elem
                     then
                        True
                     else
                        myElem elem xs


-- QUESTION 5
longestPrefix :: Eq a => [a] -> [a] -> [a]
longestPrefix [] [] = []


-- QUESTION 6
mccarthy_91 :: Int -> Int


-- QUESTION 7
listMinToMax :: Int -> Int -> [Int]


