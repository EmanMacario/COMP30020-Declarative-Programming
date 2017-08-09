-- COMP30020: Week 2 Workshop Question Solutions
-- Date: 9/08/17
-- Author: Emmanuel Macario <macarioe>

-- QUESTION 1
{- 
1. (Integer, String) 2-tuple
    Disadavantages: Able to construct 'impossible' cards

-}


-- QUESTION 2
data FontColour = ColourName String
                | Hex Int
                | RGB Int Int Int

data FontTag = FontTag (Maybe Int) (Maybe String) (Maybe FontColour)


{- 
    Can do same thing using record syntax 

    type FontSize = Int
    type FontFace = String
    data Colour = CName String | CHex Int | CRGB Int Int Int

    data HTMLFont = HTMLFont {
        fontSize   :: Maybe FontSize
      , fontFace   :: Maybe FontFace
      , fontColour :: Maybe Colour
    }
    
-}


-- QUESTION 3
factorial :: Int -> Int
factorial n
    | n < 0  = error "'n' must be a positive integer"
    | n == 0 = 1
    | n > 0  = n * factorial (n-1)

factorial' n = product [1..n]


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
longestPrefix [] _ = []
longestPrefix _ [] = []
longestPrefix xs ys 
    | head xs == head ys = [head xs] ++ longestPrefix (tail xs) (tail ys)
    | otherwise          = []



-- QUESTION 6
mccarthy_91 :: Int -> Int -> Int
mccarthy_91 c n
    | c /= 0    = if n > 100 
                  then 
                      mccarthy_91 (c-1) (n-10)
                  else
                      mccarthy_91 (c+1) (n+11)
    | otherwise = n


-- QUESTION 7
listMinToMax :: Int -> Int -> [Int]
listMinToMax x y
    | x == y = [x]
    | x < y  = [x] ++ listMinToMax (x+1) y
    | x > y  = [y] ++ listMinToMax x (y+1)