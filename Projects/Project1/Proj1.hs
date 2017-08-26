{-----------------------------------------------------------------------------

    File: Proj1.hs
    Name: Emmanuel Macario <macarioe>
    Student Number: 831659
    Origin: 19:00 Fri 18 Aug 2017

    Purpose: The purpose of this module is to implement the guessing part
    of the logical guessing game, named 'ChordProbe'.

------------------------------------------------------------------------------}

module Proj1 (initialGuess, nextGuess, GameState) where


import Data.List


-- A standard note. Notes are ordered alphabetically.

data Note = A | B | C | D | E | F | G
          deriving (Eq, Ord, Bounded, Enum)

notechars = "ABCDEFG"

instance Show Note where
    show n = [notechars !! fromEnum n]



-- A standard octave. Octaves are in the range 1-3, 
-- where 1 is the lowest octave and 3 is the highest.

data Octave = One | Two | Three
            deriving (Eq, Ord, Bounded, Enum)

octavechars = "123"

instance Show Octave where
    show o = [octavechars !! fromEnum o]



-- A standard pitch. Flats and sharps are not included.
-- Each pitch contains one standard note and one standard 
-- octave.

data Pitch = Pitch { 
               note   :: Note
             , octave :: Octave
             } deriving (Eq, Ord, Bounded)


instance Show Pitch where
    show (Pitch n o) = show n ++ show o


instance Enum Pitch where
    fromEnum (Pitch n o) = (length octavechars) * (fromEnum n) + (fromEnum o)
    toEnum int = (Pitch n o) 
        where 
            n = toEnum $ int `div` (length octavechars)
            o = toEnum $ int `mod` (length octavechars)



-- 'TargetChords' Type, which contains all remaining possible chords,
-- derived from both previous guesses and previous answers.
type SearchSpace = [[String]]


-- 'PreviousGuesses' Type is a list of chords, used for holding
-- previous guesses.
type PrevGuesses = [[String]]


-- 'PreviousAnswers' Type is a list of 3-tuples of integers, 
-- used for holding answers to previous guesses.
type PrevAnswers = [(Int, Int, Int)]


-- 'GameState' type holds the current state of the game
data GameState = GameState {
                 targets     :: SearchSpace
               , prevGuesses :: PrevGuesses
               , prevAnswers :: PrevAnswers
               }

-------------------------------------------------------------------------------


-- TESTING SOME SHIT OUT
p1 = Pitch A One
p2 = Pitch G Two
p3 = Pitch D Three


-- (1,2,1)
x1 = ["A1","B2","A3"]
y1 = ["A1","A2","B1"]

-- (1,0,2)
x2 = ["A1","B2","C3"]
y2 = ["A1","A2","A3"]

-- (0,1,2)
x3 = ["A1","B1","C1"]
y3 = ["A2","D1","E1"]

-- (0,3,3)
x4 = ["A3","B2","C1"]
y4 = ["C3","A2","B1"]


-- Enumerate a list of all possible pitches
allPitches = [minBound..maxBound] :: [Pitch]


-- Enumerates a search space consisting of all possible 
-- target chords. Ensures that there are no duplicates.
createSearchSpace :: [Pitch] -> SearchSpace
createSearchSpace allPitches = nub $ map sort allChords
        where allChords = 
                [ map show [p1, p2, p3] | 
                        p1 <- allPitches,
                        p2 <- filter (\p -> p /= p1) allPitches,
                        p3 <- filter (\p -> p /= p1 && p /= p2) allPitches ]



-- Create the initial 'search space', consisting of all possible chords.
searchSpace = createSearchSpace allPitches 


-- Now, initialise the 'GameState' before the game begins.
state = GameState {
        targets     = searchSpace
      , prevGuesses = []
      , prevAnswers = []
}



-- Takes no input arguments, and returns a 2-tuple, including 
-- an initial guess chord and the intial game state.
initialGuess :: ([String], GameState)
initialGuess = (["A1","B2","C3"], state)


-- Takes as input a pair of the previous guess and game state, and the feedback
-- to this guess as a triple of correct pitches, notes, and octaves, and returns
-- a pair of the next guess and game state.
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (lastGuess, state) lastAnswer = (newGuess, newState)
    where
        newState = updateGameState state lastGuess lastAnswer
        newGuess = head $ targets newState



-- Updates the state of the game, given the current game state,
-- previous guess, and answer to the previous guess.
updateGameState :: GameState -> [String] -> (Int, Int, Int) -> GameState
updateGameState state lastGuess lastAnswer = newState
    where newState = GameState {
                      targets = reduceSearchSpace lastGuess lastAnswer (tail (targets state))
                   ,  prevGuesses = lastGuess : prevGuesses state
                   ,  prevAnswers = lastAnswer : prevAnswers state
                   }


-- Reduces the search space, by removing target chords that do 
-- not align with the received answers from previous guesses.

reduceSearchSpace :: [String] -> (Int, Int, Int) -> SearchSpace -> SearchSpace
reduceSearchSpace lastGuess lastAnswer searchSpace = 
    [ newGuess | newGuess <- searchSpace, 
                 isValid lastGuess newGuess lastAnswer ]


-- Computes the answer to a guess. The first argument is the target, the second
-- argument is the guess. The third argument is the matching score.
isValid :: [String] -> [String] -> (Int, Int, Int) -> Bool
isValid lastGuess newGuess lastAnswer = 
                            response lastGuess newGuess == lastAnswer 



intersectNoDuplicates ::Eq a => [a] -> [a] -> [a]
intersectNoDuplicates xs ys = xs \\ (xs \\ ys)



-- Computes the answer to a guess. First argument is the 
-- target, second is the guess.
answer :: [String] -> [String] -> (Int, Int, Int)
answer targetChord lastGuess = (correct, correctNotes, correctOctaves)
    where correct = length $ intersect targetChord lastGuess

          num     = length lastGuess

          x = lastGuess \\ (intersect targetChord lastGuess)
          y = targetChord \\ (intersect targetChord lastGuess)


          guessNotes     = map (!! 0) x
          targetNotes    = map (!! 0) y

          guessOctaves   = map (!! 1) x
          targetOctaves  = map (!! 1) y

          correctNotes   = 3 - length (deleteIntersect targetNotes guessNotes)
          correctOctaves = 3 - length (deleteIntersect targetOctaves guessOctaves)





deleteIntersect :: [Char] -> [Char] -> [Char]
deleteIntersect xs []  = xs
deleteIntersect xs ys = if elem (head ys) xs 
                        then
                            deleteIntersect (xs \\ [head ys]) (tail ys)
                        else
                            deleteIntersect xs (tail ys)



-- | Compute the correct answer to a guess.  First argument is the 
--   target, second is the guess.
response :: [String] -> [String] -> (Int,Int,Int)
response target guess = (right, rightNote, rightOctave)
  where guess'      = nub guess
        right       = length $ intersect guess' target
        num         = length guess'
        rightNote   = num - (length $ deleteFirstsBy (eqNth 0) guess' target) 
                    - right
        rightOctave = num - (length $ deleteFirstsBy (eqNth 1) guess' target) 
                    - right


-- | eqNth n l1 l2 returns True iff element n of l1 is equal to 
--   element n of l2.
eqNth :: Eq a => Int -> [a] -> [a] -> Bool
eqNth n l1 l2 = (l1 !! n) == (l2 !! n)




-- Takes target chord and a guess and returns intersect
getIntersect :: [Char] -> [Char] -> [Char]
getIntersect [] _ = []
getIntersect xs ys = 
    if head xs `elem` ys
    then 
        getIntersect (tail xs) ys
    else
        head xs : getIntersect xs ys





-- Function to rank target chords with respect to guess optimality

