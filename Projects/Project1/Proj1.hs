{-----------------------------------------------------------------------------

    COMP30020 Project 1 - The Game of 'ChordProbe'

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



-- FINISH OFF DERIVING AN INSTANCE FOR READ


instance Enum Pitch where
    fromEnum (Pitch n o) = (length octavechars) * (fromEnum n) + (fromEnum o)
    toEnum int = (Pitch n o) 
        where 
            n = toEnum $ int `div` (length octavechars)
            o = toEnum $ int `mod` (length octavechars)



-- A standard chord. Includes three distinct standard pitches.
data Chord = Chord {
             pitchOne   :: Pitch
           , pitchTwo   :: Pitch
           , pitchThree :: Pitch
           } deriving (Eq)



-- TESTING SOME SHIT OUT
p1 = Pitch A One
p2 = Pitch G Two
p3 = Pitch D Three

chord = Chord p1 p2 p3


-- Enumerate a list of all possible pitches
allPitches = [minBound..maxBound] :: [Pitch]


-- Enumerates a search space consisting of all possible 
-- target chords. Ensures that there are no duplicates.
createSearchSpace :: [Pitch] -> SearchSpace
createSearchSpace allPitches = nub $ map sort allChords
        where allChords = 
                [ 
                    [show p1, show p2, show p3] 
                        | p1 <- allPitches,
                          p2 <- filter (\p -> p /= p1) allPitches,
                          p3 <- filter (\p -> p /= p1 && p /= p2) allPitches
                ]




-- 'TargetChords' Type, which contains all remaining possible chords,
-- derived from both previous guesses and previous answers.
type SearchSpace = [[String]]


-- 'PreviousGuesses' Type is a list of chords, used for holding
-- previous guesses.
type PreviousGuesses = [[String]]


-- 'PreviousAnswers' Type is a list of 3-tuples of integers, 
-- used for holding answers to previous guesses.
type PreviousAnswers = [(Int, Int, Int)]


-- 'GameState' type holds the current state of the game
data GameState = GameState {
                 targets         :: SearchSpace
               , previousGuesses :: PreviousGuesses
               , previousAnswers :: PreviousAnswers
               }




searchSpace = createSearchSpace allPitches 


-- Now, initialise the 'GameState' before the game begins.
gamestate = GameState {
    targets = tail searchSpace,
    previousGuesses = [],
    previousAnswers = []
}



-- Takes no input arguments, and returns a 2-tuple, including 
-- an initial guess chord and the intial game state.
initialGuess :: ([String], GameState)
initialGuess = (head searchSpace, gamestate)


-- Takes as input a pair of the previous guess and game state, and the feedback
-- to this guess as a triple of correct pitches, notes, and octaves, and returns
-- a pair of the next guess and game state.
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess (lastGuess, gamestate) (p,n,o) = 
                    (head $ targets gamestate, 
                            updateGameState gamestate lastGuess (p,n,o))




-- Updates the state of the game.
updateGameState :: GameState -> [String] -> (Int, Int, Int) -> GameState
updateGameState gamestate lastGuess lastAnswer = newgamestate
    where newgamestate = GameState {
                            targets = tail $ targets gamestate
                         ,  previousGuesses = lastGuess : previousGuesses gamestate
                         ,  previousAnswers = lastAnswer : previousAnswers gamestate
                         }



-- Reduces the search space, by removing target chords that do 
-- not align with the received answers from previous guesses.
-- reduceSearchSpace :: [String] -> (Int, Int, Int) -> SearchSpace -> SearchSpace



-- Function to rank target chords with respect to guess optimality