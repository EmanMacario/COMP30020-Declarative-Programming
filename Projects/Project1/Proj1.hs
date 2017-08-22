{-----------------------------------------------------------------------------

    COMP30020 Project 1 - The Game of 'ChordProbe'

    File: Proj1.hs
    Name: Emmanuel Macario <macarioe>
    Student Number: 831659
    Origin: 19:00 Fri 18 Aug 2017

    Purpose: The purpose of this module is to implement the guessing part
    of a logical guessing game, named 'ChordProbe'.

------------------------------------------------------------------------------}

module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List
import qualified Data.Set as Set


-- Basic Types for a 'Pitch'
type Note   = Char
type Octave = Char

-- 'Pitch' Type, containing one 'Note' and one 'Octave'
data Pitch = Pitch {
  note   :: Note
, octave :: Octave
} deriving (Show, Eq)

-- 'Chord' Type, containing three distinct instances of a 'Pitch'
data Chord = Chord { 
  p1  :: Pitch
, p2  :: Pitch
, p3  :: Pitch
} deriving (Show, Eq)


-- 'TargetChords' Type, which contains all remaining possible chords,
-- derived from both previous guesses and previous answers.
type SearchSpace = [[String]]


-- 'PreviousGuesses' Type is a list of chords, used for holding
-- previous guesses.
type PreviousGuesses = [Chord]


-- 'PreviousAnswers' Type is a list of 3-tuples of integers, used for
-- holding answers to previous guesses.
type PreviousAnswers = [(Int, Int, Int)]


-- 'GameState' type holds the current state of the game
data GameState = GameState {
    targets :: SearchSpace
}

-- Testing syntax
targetChords =  [
                    Chord 
                    { 
                      p1 = Pitch {note = 'A', octave = '1'}
                    , p2 = Pitch {note = 'B', octave = '3'}
                    , p3 = Pitch {note = 'C', octave = '2'}
                    },

                    Chord {
                        p1 = Pitch {note = 'G', octave = '1'}
                    ,   p2 = Pitch {note = 'F', octave = '3'}
                    ,   p3 = Pitch {note = 'C', octave = '1'}
                    }
                ]


-- Valid notes and octaves.
noteChars = "ABCDEFG"
octaveChars = "123"

-- Enumerates all possible pitches.
allPitches = [ [p, o] | p <- noteChars, o <- octaveChars ]

-- Enumerates all possible target chords prior to game start, ensuring
-- that there are no duplicates.
searchSpace = nub $ map sort allChords
    where allChords =                                                            -- Note:
            [ [p1, p2, p3] | p1 <- allPitches,                                   -- Could just change [p1,p2,p3] if you wanted
                             p2 <- filter (\p -> p /= p1) allPitches,            -- to use another type instead of list to store
                             p3 <- filter (\p -> p /= p1 && p /= p2) allPitches  -- target chords.
            ]



-- Now, initialise the 'GameState' before the game begins.
gamestate = GameState {
    targets = tail searchSpace
}



-- Takes no input arguments, and returns a 2-tuple, including 
-- an initial guess chord and the intial game state.
initialGuess :: ([String], GameState)
initialGuess = (head searchSpace, gamestate)


-- Takes as input a pair of the previous guess and game state, and the feedback
-- to this guess as a triple of correct pitches, notes, and octaves, and returns
-- a pair of the next guess and game state.
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess ((c1:c2:c3:[]), gamestate) (p,n,o) = (head $ targets gamestate, 
                                                updateGameState gamestate)




updateGameState :: GameState -> GameState
updateGameState gamestate = newgamestate
    where newgamestate = GameState {
                            targets = tail $ targets gamestate
                         }



-- Function to reduce down possible answers



-- Function to rank possible answers



{- 

GAME IDEAS:



-}