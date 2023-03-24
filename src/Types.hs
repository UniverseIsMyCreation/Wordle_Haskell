module Types where

type FileName = String
type UserWord = String
type CodeAnswer = String

data History a = 
    EmptyHistory -- Нету введенных слов от пользователя
    | History [(a, a)] -- Список пар: Слово - Код похожести на ответ
        deriving (Show)

data GameState = GameState 
    { word :: String
    , guesses :: History String
    , attemtps :: Int
    , win :: Bool
    , lose :: Bool
    , wordLen :: Int
    } deriving Show -- Игровое состояние

data Dictionary a = 
    EmptyDictionary -- Пустой словарь для игры 
    | Dictionary [a] -- Список слов Словаря
        deriving (Show)