module Preprocessing(
    readDictionary,
    getRandomIndex,
    randomWord,
    checkGuessWord
) where

import System.Random (randomRIO)
import Data.List.Split
import System.IO()
import Data.Char (toLower)

import Constants
import Types

readDictionary :: FileName -> IO (Dictionary String) -- Read Dictionary
readDictionary path = do
    contents <- readFile path
    let ourDictionary = Dictionary $ splitOn " " contents
    let ourCleanDictionary = cleanDictionary ourDictionary
    let ourUpperDictionary = downDictionary ourCleanDictionary
    return $ ourUpperDictionary
    where
        cleanDictionary :: Dictionary String -> Dictionary String -- Get rid of empty words
        cleanDictionary EmptyDictionary = EmptyDictionary
        cleanDictionary (Dictionary []) = Dictionary []
        cleanDictionary (Dictionary (x:xs)) = Dictionary $ cleanList (x:xs) where
            cleanList :: [String] -> [String]
            cleanList [] = []
            cleanList (y:ys) | (length y) == wordLength = y : cleanList ys
                             | otherwise = cleanList ys
        
        downDictionary :: Dictionary String -> Dictionary String -- to lower case
        downDictionary EmptyDictionary = EmptyDictionary
        downDictionary (Dictionary []) = Dictionary []
        downDictionary (Dictionary lst) = Dictionary $ downList lst where
            downList :: [String] -> [String]
            downList [] = []
            downList (x:xs) = (map toLower x) : downList xs 

getRandomIndex :: Dictionary [a] -> IO (Int) -- Get random index for word
getRandomIndex ourDictionary = do
    index <- randomRIO (0, (getDictionaryLength ourDictionary - 1))
    return $ index
    where
        getDictionaryLength :: Dictionary [a] -> Int -- Get dictionaty length
        getDictionaryLength EmptyDictionary = 0
        getDictionaryLength (Dictionary []) = 0
        getDictionaryLength (Dictionary (x:xs)) = length (x:xs)

randomWord :: Dictionary String -> Int -> Either Int UserWord -- Generate random word to guess
randomWord ourDictionary posWord = case ourDictionary of
    EmptyDictionary -> Left $ -1
    Dictionary [] -> Left $ -1
    Dictionary (x:xs) -> if length (x:xs) < minLenDict then Left $ -1 else getWord (x:xs) posWord 0
    where
        getWord :: [String] -> Int -> Int -> Either Int UserWord -- Get word
        getWord [] _ _ = Left $ -1
        getWord (x:xs) findWordPos curPos | findWordPos == curPos = Right x
                                          | otherwise = getWord xs findWordPos (curPos + 1)

checkGuessWord :: Either Int UserWord -> IO () -- Check correctness of our dictionary
checkGuessWord x = case (checkEither x) of
    0 -> anounceError "There are some problems with Dictionary!!!"
    _ -> anounceJust "Everything is okay"
    where 
        checkEither :: Either Int UserWord -> Int -- Check
        checkEither (Left _) = 0
        checkEither (Right _) = roundCounts
        anounceError :: String -> IO () -- Announce about error
        anounceError localError = do
            putStrLn localError
        anounceJust :: String -> IO () -- Anmounce about correctness
        anounceJust local = do
            putStrLn local
