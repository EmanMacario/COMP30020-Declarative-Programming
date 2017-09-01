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

noteChars = ['A'..'G']

instance Show Note where
    show n = [noteChars !! fromEnum n]



-- A standard octave. Octaves are in the range 1-3, 
-- where 1 is the lowest octave and 3 is the highest.
data Octave = One | Two | Three
            deriving (Eq, Ord, Bounded, Enum)

octaveChars = ['1'..'3']

instance Show Octave where
    show o = [octaveChars !! fromEnum o]



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
    fromEnum (Pitch n o) = (length octaveChars) * (fromEnum n) + (fromEnum o)
    toEnum int = (Pitch n o) 
        where 
            n = toEnum $ int `div` (length octaveChars)
            o = toEnum $ int `mod` (length octaveChars)


-- 'GameState' type holds the current state of the game, 
-- including all possible target chords left.
data GameState = GameState { targets :: [[String]] }


-- Enumerates a search space consisting of all possible 
-- target chords. Ensures that there are no duplicates.
createSearchSpace :: [[String]]
createSearchSpace = allChords
    where 
       allPitches = [minBound..maxBound] :: [Pitch]
       allChords  = [ map show [p1,p2,p3] | p1 <- allPitches, p2 <- allPitches,
                                            p3 <- allPitches, p1 < p2, p2 < p3 ]


-- Initialises the gamestate for a new game.
initialiseGameState :: GameState
initialiseGameState = newGameState
    where
        newGameState = GameState { targets = createSearchSpace }



-- Takes no input arguments, and returns a 2-tuple, including 
-- an initial guess chord and the intial game state. 
-- Note the guess ["A1","B1","C2"] is the lexicographically least, 
-- and most optimal initial guess.
initialGuess :: ([String], GameState)
initialGuess = (["A1","B1","C2"], initialiseGameState)


-- Takes as input a pair of the previous guess and game state, and the feedback
-- to this guess as a 3-tuple of correct pitches, notes, and octaves, and 
-- returns a 2-tuple containing the next guess and updated game state.
nextGuess :: ([String], GameState) -> (Int,Int,Int) -> ([String],GameState)
nextGuess (lastGuess, state) lastAnswer = (newGuess, newState)
    where
        newState   = updateGameState state lastGuess lastAnswer
        allTargets = miniMax' (targets newState) (targets newState)
        newGuess   = getOptimalGuess' allTargets


-- Updates the state of the game, given the current game state,
-- previous guess, and answer to the previous guess.
updateGameState :: GameState -> [String] -> (Int,Int,Int) -> GameState
updateGameState state lastGuess lastAnswer = newState
    where 
        newTargets = reduceSearchSpace lastGuess lastAnswer $ targets state
        newState   = GameState { targets = newTargets }


-- Reduces the search space, by removing target chords that do 
-- not align with the received answers from previous guesses.
reduceSearchSpace :: [String] -> (Int,Int,Int) -> [[String]] -> [[String]]
reduceSearchSpace lastGuess lastAnswer searchSpace = updatedSearchSpace
    where
        updatedSearchSpace = [ chord | chord <- searchSpace, 
                                       isValid lastGuess chord lastAnswer ]


-- Computes the answer to a guess. The first argument is the target, the second
-- argument is the guess. The third argument is the matching score.
isValid :: [String] -> [String] -> (Int,Int,Int) -> Bool
isValid lastGuess newGuess lastAnswer = answer lastGuess newGuess == lastAnswer 


-- Computes the answer to a guess. First argument 
-- is the target, second is the guess.
answer :: [String] -> [String] -> (Int,Int,Int)
answer targetChord lastGuess = (correct, correctNotes, correctOctaves)
    where 
        samePitches       = intersect targetChord lastGuess
        correct           = length $ samePitches

        -- Leftover notes from pitches uncommon to both
        -- target and guess chords.
        leftTargetNotes   = map (!! 0) (targetChord \\ samePitches)
        leftGuessNotes    = map (!! 0) (lastGuess \\ samePitches)

        -- Leftover octaves from pitches uncommon to both
        -- target and guess chords.
        leftTargetOctaves = map (!! 1) (targetChord \\ samePitches)
        leftGuessOctaves  = map (!! 1) (lastGuess \\ samePitches)

        correctNotes      = length leftTargetNotes - 
                                length (leftTargetNotes \\ leftGuessNotes)
        correctOctaves    = length leftTargetOctaves -
                                length (leftTargetOctaves \\ leftGuessOctaves)




-- Apply 'minimax' technique to find the next optimal guess. This is achieved
-- by computing for each possible target, the minimum number of possible chords
-- that it may eliminate from the search space.


-- Given a target chord, and a list of all remaining possible guesses,
-- returns the expected average number of remaining chords.
countDistinctAnswers :: Fractional a => [String] -> [[String]] -> a
countDistinctAnswers target searchSpace = avgGuesses
    where 
        total = length searchSpace
        answers = sort $ map (answer target) [ guess | guess <- searchSpace ]
        distinctAnswerCounts = map ((\n -> n * n) . length) $ group answers
        avgGuesses = (fromIntegral $ sum distinctAnswerCounts) / 
                                                        (fromIntegral total) 



-- Returns the guess that will leave the minimum maximum possible targets.
miniMax' :: Fractional a => [[String]] -> [[String]] -> [(a,[String])] 
miniMax' [] _ = []
miniMax' targets searchSpace = 
    let target = head targets
        avgTargets = (countDistinctAnswers target searchSpace, target)
    in
        avgTargets : miniMax' (tail targets) searchSpace


getOptimalGuess' :: Fractional a => Ord a => [(a,[String])] -> [String]
getOptimalGuess' allGuesses = snd $ foldl1 min allGuesses


-------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
