-- COMP30020 Project 1 - The Game of 'ChordProbe'
--
-- File: Proj1.hs
-- Name: Emmanuel Macario <macarioe>
-- Student Number: 831659
-- Origin: 19:00 Fri 18 Aug 2017

-- Purpose: The purpose of this module is to implement tge guessing part
-- of a logical guessing game. 


module Proj1 (initialGuess, nextGuess, GameState) where


-- Type indicates no information is to be stored about game state.
type GameState = ()


-- Takes no input arguments, and returns a pair of an 
-- initial guess and a game state.
initialGuess :: ([String], GameState)
initialGuess = (["A1", "B2", "C3"], ())


-- Takes as input a pair of the previous guess and game state, and the feedback
-- to this guess as a triple of correct pitches, notes, and octaves, and returns
-- a pair of the next game and game state.
nextGuess :: ([String], GameState) -> (Int, Int, Int) -> ([String], GameState)
nextGuess ((a:b:c:[]), state) (x,y,z) = (["A3", "B2", "G2"], ())