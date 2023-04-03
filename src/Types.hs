module Types(
    History(EmptyHistory, History),
    GameState(GameState),
    Dictionary(EmptyDictionary, Dictionary),
    word,
    guesses,
    attemtps,
    win,
    lose,
    wordLen,
    FileName,
    UserWord,
    CodeAnswer
) where

type FileName = String
type UserWord = String
type CodeAnswer = String

data History a = 
    EmptyHistory -- There is not input data
    | History [(a, a)] -- Pairs' list: word - similarity code
        deriving (Show)

data GameState = GameState 
    { word :: String
    , guesses :: History String
    , attemtps :: Int
    , win :: Bool
    , lose :: Bool
    , wordLen :: Int
    } deriving Show -- Gamestate

data Dictionary a = 
    EmptyDictionary -- Empty dictionary for the game
    | Dictionary [a] -- Words' dictionary list 
        deriving (Show)