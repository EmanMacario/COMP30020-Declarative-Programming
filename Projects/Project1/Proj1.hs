--   File     : Proj1.hs
--   Author   : Emmanuel Macario <macarioe> <831659>
--   Origin   : 19:00 Fri 18 Aug 2017
-- 
--   Purpose  : To implement the guessing part of the logical 
--              guessing game, named 'ChordProbe'.
--
--
--   This code contains a variety of data types and auxiliary functions
--   to help implement the 'initialGuess' and 'nextGuess' functions, used 
--   by the performer in the game 'ChordProbe'. The purpose of these 
--   functions is to reduce symmetry in the problem space, in an attempt 
--   to minimise the average number of guesses per target, while also
--   trying to minimise worst case run-time complexity.


module Proj1 (initialGuess, nextGuess, GameState) where

import Data.List

----
---
--       Code for implementation of data types used to create a chord,
--              and data structures used for the GameState type.
--
--       Note that all types created for the chord are in the Bounded, 
---                  Enum, Ord, Eq and Show type classes.
----

-- A standard note. Notes are ordered alphabetically.
data Note = A | B | C | D | E | F | G
          deriving (Enum, Bounded, Ord, Eq)

noteChars = ['A'..'G']

instance Show Note where
    show note = [noteChars !! fromEnum note]



-- A standard octave. Octaves are in the range 1 to 3, 
-- where 1 is the lowest octave and 3 is the highest.
data Octave = One | Two | Three
            deriving (Enum, Bounded, Ord, Eq)

octaveChars = ['1'..'3']

instance Show Octave where
    show octave = [octaveChars !! fromEnum octave]



-- A standard pitch. Flats and sharps are not included.
-- Each pitch contains one standard note and one standard 
-- octave.
data Pitch = Pitch { note :: Note, octave :: Octave } 
           deriving (Bounded, Ord, Eq)

instance Show Pitch where
    show (Pitch note octave) = show note ++ show octave

instance Enum Pitch where
    fromEnum (Pitch note octave) = (length octaveChars) * (fromEnum note) 
                                                        + (fromEnum octave)
    toEnum n = (Pitch note octave) 
        where 
            note   = toEnum $ n `div` (length octaveChars)
            octave = toEnum $ n `mod` (length octaveChars)



-- Note: it has been decided to represent a chord as a list of
-- Strings, instead of a more complicated data type/structure. This is
-- purposely done to avoid unecessary conversions between character 
-- representations of a chord, and normal Haskell data structures.


-- 'GameState' type holds the current state of the game, 
-- including all possible target chords left.
data GameState = GameState { targets :: [[String]] }



----
---
--        Code for the implementation of the 'initialGuess' function.
---
----

-- Enumerates a search space consisting of all possible 
-- target chords. Ensures that there are no duplicates.
createSearchSpace :: [[String]]
createSearchSpace = allChords
    where 
       allPitches = [minBound..maxBound] :: [Pitch]
       allChords  = [ map show [p1,p2,p3] | p1 <- allPitches, p2 <- allPitches,
                                            p3 <- allPitches, p1 < p2, p2 < p3 ]



-- Initialises the game state for a new game.
initialiseGameState :: GameState
initialiseGameState = newGameState
    where
        newGameState = GameState { targets = createSearchSpace }



-- Returns a 2-tuple, where the first element is the initial 
-- guess, and the second element is the initial game state. Note 
-- that the guess ["A1","B1","C2"] is the lexicographically 
-- least, most optimal initial guess.
initialGuess :: ([String], GameState)
initialGuess = (["A1","B1","C2"], initialiseGameState)



----
---
--          Code for the implementation of the 'nextGuess' function.
---
----

-- Own implementation of the 'response' function in 'Proj1Test.hs'.
-- Computes the correct answer for a given guess. The first 
-- argument is the target, the second argument is the guess.
response' :: [String] -> [String] -> (Int,Int,Int)
response' target guess = (correctPitches, correctNotes, correctOctaves)
    where 
        -- Firstly, compute number of correct pitches in the guess.
        samePitches       = intersect target guess
        correctPitches    = length samePitches

        -- Secondly, compute number of pitches in the guess 
        -- with the correct note, but wrong octave.
        leftTargetNotes   = map (!! 0) (target \\ samePitches)
        leftGuessNotes    = map (!! 0) (guess \\ samePitches)
        correctNotes      = length leftTargetNotes - 
                                length (leftTargetNotes \\ leftGuessNotes)

        -- Lastly, compute number of pitches in the guess 
        -- with the correct octave, but wrong note.
        leftTargetOctaves = map (!! 1) (target \\ samePitches)
        leftGuessOctaves  = map (!! 1) (guess \\ samePitches)
        correctOctaves    = length leftTargetOctaves -
                                length (leftTargetOctaves \\ leftGuessOctaves)



-- Returns a boolean, indicating whether the answer 
-- computed for a guess and possible target is the 
-- same as the actual answer received for that guess.
isValid :: [String] -> [String] -> (Int,Int,Int) -> Bool
isValid guess target answer = response' guess target == answer 



-- Returns a reduced search space, obtained by removing
-- remaining possible target chords that are inconsistent
-- with the answer received for the previous guess.
reduceSearchSpace :: [String] -> (Int,Int,Int) -> [[String]] -> [[String]]
reduceSearchSpace guess answer searchSpace = updatedSearchSpace
    where
        updatedSearchSpace = [ target | target <- searchSpace, 
                                        isValid guess target answer ]



-- Updates the state of the game, given the current game state,
-- previous guess, and answer to the previous guess.
updateGameState :: GameState -> [String] -> (Int,Int,Int) -> GameState
updateGameState state guess answer = updatedState
    where 
        updatedSearchSpace = reduceSearchSpace guess answer $ targets state
        updatedState       = GameState { targets = updatedSearchSpace }



-- Given a candidate target chord and the current search space,
-- returns the expected (average) number of remaining chords in
-- the search space if that guess were to be made. This is done by 
-- collecting all distinct answers for a given guess and for each 
-- answer, counting the number of targets that gave that answer. 
-- From this, the expected average number of remaining targets can
-- be calculated. 
expectedRemaining :: Fractional a => [String] -> [[String]] -> a
expectedRemaining guess searchSpace = expectedAverage
    where 
        -- Get total number of possible targets remaining.
        nTargets = length searchSpace

        -- Get all answers for a guess.
        answers = map (response' guess) [ target | target <- searchSpace ]

        -- Sort and group the answers. The length of a group
        -- specifies the number of targets that gave that answer.
        distinctAnswers = group $ sort answers

        -- Now perform some calculations to get the expected
        -- average remaining targets for the guess.
        total = sum $ map ((\n -> n * n) . length) distinctAnswers
        expectedAverage = fromIntegral total / fromIntegral nTargets



-- Returns the most optimal guess given the current game state,
-- by calculating the expected number of remaining targets
-- for each possible guess, and choosing the guess that will
-- leave the minimum expected number of remaining candidates.
getOptimalGuess :: GameState -> [String]
getOptimalGuess state = optimalGuess
    where
        searchSpace  = targets state
        allGuesses   = [ (expectedRemaining target searchSpace, target) 
                                                | target <- searchSpace ]

        optimalGuess = snd $ foldl1 min allGuesses



-- Takes as input a 2-tuple containing the previous guess and current
-- game state, and the answer to the previous guess. Returns a 2-tuple 
-- containing the next guess and updated game state.
nextGuess :: ([String], GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (lastGuess, state) lastAnswer = (newGuess, newState)
    where
        newState = updateGameState state lastGuess lastAnswer
        newGuess = getOptimalGuess newState