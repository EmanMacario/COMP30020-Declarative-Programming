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


-- Basic Types for a 'Pitch'
type Note   = Char
type Octave = Char

-- 'Pitch' Type, containing one 'Note' and one 'Octave'
data Pitch = Pitch {
  note   :: Note
, octave :: Octave
} deriving (Show)

-- 'Chord' Type, containing three distinct instances of a 'Pitch'
data Chord = Chord {
     p1   :: Pitch
,    p2   :: Pitch
,    p3   :: Pitch
} deriving (Show)


-- 'TargetChords' Type, which contains all remaining possible chords,
-- derived from both previous guesses and previous answers.
type TargetChords = [Chord]


-- 'PreviousGuesses' Type is a list of 3-tuples of integers, used for holding
-- answers from previous guesses.
type PreviousGuesses = [(Int, Int, Int)]


-- 'GameState' type holds the current state of the game
data GameState = GameState TargetChords 


-- Testing syntax
targetChords =  [
                    Chord {
                        p1 = Pitch {note = 'A', octave = '1'}
                    ,   p2 = Pitch {note = 'B', octave = '3'}
                    ,   p3 = Pitch {note = 'C', octave = '2'}
                    },

                    Chord {
                        p1 = Pitch {note = 'G', octave = '1'}
                    ,   p2 = Pitch {note = 'F', octave = '3'}
                    ,   p3 = Pitch {note = 'C', octave = '1'}
                    }
                ]



-- Takes no input arguments, and returns a 2-tuple, including 
-- an initial guess chord and the intial game state.
initialGuess :: ([String], GameState)
initialGuess = (["A1", "B2", "C3"], GameState targetChords)


-- Takes as input a pair of the previous guess and game state, and the feedback
-- to this guess as a triple of correct pitches, notes, and octaves, and returns
-- a pair of the next guess and game state.
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess ((c1:c2:c3:[]), game) (p,n,o) = (["A3", "B2", "G2"], 
                                                        GameState targetChords)






-- Function to reduce down possible answers



-- Function to rank possible answers



{- 

GAME IDEAS:

We end our guess when it is (3,0,0)


-}