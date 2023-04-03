module Game (
    startGame
) where

import System.IO()

import Gameplay
import Preprocessing

startGame :: IO () -- Launch the game
startGame = do
    putStrLn "Welcome to the world-wide famous game Worldle" -- Hello the player

    putStrLn "Local guide:"
    putStrLn "---white(0) means there is no such letter in word"
    putStrLn "---yellow(1) means correct letter for this word but not in correct position"
    putStrLn "---green(2) means correct letter"
    putStrLn "For each symbol in your word you will get combintaion"
    putStrLn "Example:"
    putStrLn "20120 = 2 greens, 2 whites, 1 yellow"

    putStrLn "Please, input file name with same length(6) words to guess"
    inputFile <- getLine
    
    ourDictionary <- readDictionary inputFile -- Get prepared dictionary
    wordIndex <- getRandomIndex ourDictionary -- Either get word index either error 
    let guessWord = randomWord ourDictionary wordIndex -- Either get word either error
    checkGuessWord guessWord -- Chech correctness of input data
    playGame guessWord -- Game
