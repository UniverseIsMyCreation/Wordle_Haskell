module Lib (
    startGame
) where

import System.Random (randomRIO)
import Data.List.Split
import System.IO()
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Data.Char (toLower)

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
    } deriving Show

data Dictionary a = 
    EmptyDictionary -- Пустой словарь для игры 
    | Dictionary [a] -- Список слов Словаря
        deriving (Show)

----------------------------------CONSTANTS----------------------------------------------------------------------------

minLenDict :: Int
minLenDict = 7

wordLength :: Int
wordLength = 5

roundCounts :: Int
roundCounts = 6

cellWidth :: Float
cellWidth = 100

cellHeight :: Float
cellHeight = 100

maxHeight :: Int
maxHeight = 300

maxWidth :: Int
maxWidth = -220

step :: Int
step = 100

maxWidthLet :: Int
maxWidthLet = -320

maxHeightLet :: Int
maxHeightLet = 390

stepLetWidth :: Int
stepLetWidth = 122

stepLetHeight :: Int
stepLetHeight = 140

initialState :: String -> GameState
initialState word = GameState word EmptyHistory roundCounts False False 0

background :: Color
background = white

screen :: Display
screen = InWindow "Wordle" (1600, 800) (40, 40)

stepsPerSecond :: Int
stepsPerSecond = 1

----------------------------------DRAW STATE----------------------------------------------------------------------------

drawBoard :: String -> Int -> [Picture] -- Создаем игровое поле
drawBoard _ 30 = []
drawBoard [] x = [figureSolid] ++ [figureWire] ++ (drawBoard [] $ x + 1) where
    figureSolid = color white $ translate xShift yShift $ rectangleSolid cellWidth cellHeight
    figureWire = translate xShift yShift $ rectangleWire cellWidth cellHeight
    xShift = fromIntegral $ maxWidth + step * (snd $ divMod x wordLength)
    yShift = fromIntegral $ maxHeight - step * (fst $ divMod x wordLength)

drawBoard (y:ys) x = [color (getColor y) $ figure] ++ (drawBoard ys $ x + 1) where
    figure = translate xShift yShift $ rectangleSolid cellWidth cellHeight
    xShift = fromIntegral $ maxWidth + step * (snd $ divMod x wordLength)
    yShift = fromIntegral $ maxHeight - step * (fst $ divMod x wordLength)
    getColor :: Char -> Color
    getColor '0' = greyN 0.5
    getColor '1' = yellow
    getColor '2' = green

drawLetters :: String -> Int -> [Picture] -- Отрисовка ввода пользователя
drawLetters [] pos = []
drawLetters (x:xs) pos = [cureLetter x] ++ drawLetters xs (pos + 1) where
    cureLetter :: Char -> Picture
    cureLetter x = color black $ scale 0.8 0.7 $ translate xShift yShift $ text $ cleanChar $ show x where
        xShift = fromIntegral $ maxWidthLet + stepLetWidth * (snd $ divMod pos wordLength)
        yShift = fromIntegral $ maxHeightLet - stepLetHeight * (fst $ divMod pos wordLength)
        cleanChar :: [Char] -> [Char]
        cleanChar [] = []
        cleanChar (x:xs) | x == '\'' = cleanChar xs
                         | otherwise = x : cleanChar xs

drawState :: GameState -> Picture -- Текущее состояние игры
drawState (GameState _ _ _ True False _) = pictures [winPic] where
    winPic = scale 0.6 0.4 $ translate (-700) 0 $ text $ "Congratulate, you win!!!"

drawState (GameState word _ _ False True _) = pictures [losePic, correctAnswer] where
    losePic = scale 0.6 0.4 $ translate (-700) 200 $ text $ "You lose!!!"
    correctAnswer = scale 0.6 0.4 $ translate (-700) (-200) $ text $ "Correct word was: " ++ show word

drawState (GameState _ EmptyHistory attemtps _ _ _) = pictures $ frames ++ [remainingPic, rules1, rules2, press11, press12, press2, press3] where
    frames = drawBoard [] 0
    remainingPic = scale 0.6 0.4 $ translate (-700) (-800) $ text $ "Guesses remaining: " ++ show attemtps
    rules1 = scale 0.3 0.3 $ translate (-2500) (1000) $ text $ "Here's instruction"
    rules2 = scale 0.3 0.3 $ translate (-2500) (800) $ text $ "for this game:"
    press11 = scale 0.3 0.3 $ translate (-2500) (600) $ text $ "1) Enter-enter"
    press12 = scale 0.3 0.3 $ translate (-2500) (450) $ text $ "the 5 letter word"
    press2 = scale 0.3 0.3 $ translate (-2500) (250) $ text $ "2) Space-cancel letter"
    press3 = scale 0.3 0.3 $ translate (-2500) (50) $ text $ "3) Letters-input letter"

drawState (GameState _ (History guesses) attemtps _ _ _) = pictures $ frames ++ letters ++ [remainingPic, rules1, rules2, press11, press12, press2, press3] where
    frames = drawBoard (revealListCode guesses) 0 where
        revealListCode :: [(String, String)] -> String
        revealListCode [] = []
        revealListCode (x:xs) = snd x ++ revealListCode xs
    letters = drawLetters (revealListSymbols guesses) 0 where
        revealListSymbols :: [(String, String)] -> String
        revealListSymbols [] = []
        revealListSymbols (x:xs) = fst x ++ revealListSymbols xs
    remainingPic = scale 0.6 0.4 $ translate (-700) (-800) $ text $ "Guesses remaining: " ++ show attemtps
    rules1 = scale 0.3 0.3 $ translate (-2500) (1000) $ text $ "Here's instruction"
    rules2 = scale 0.3 0.3 $ translate (-2500) (800) $ text $ "for this game:"
    press11 = scale 0.3 0.3 $ translate (-2500) (600) $ text $ "1) Enter-enter"
    press12 = scale 0.3 0.3 $ translate (-2500) (450) $ text $ "the 5 letter word"
    press2 = scale 0.3 0.3 $ translate (-2500) (250) $ text $ "2) Space-cancel letter"
    press3 = scale 0.3 0.3 $ translate (-2500) (50) $ text $ "3) Letters-input letter"

----------------------------------HANDLE USER ACTIONS----------------------------------------------------------------------------

upgradeStateLet :: GameState -> Char -> GameState -- Добавить новую букыу в историю
upgradeStateLet (GameState a1 EmptyHistory rC a2 a3 x) guess = GameState a1 (History [([guess], "0")]) rC a2 a3 (x + 1)
upgradeStateLet (GameState a1 (History lst) rC a2 a3 x) guess = GameState a1 (History (addSymbol lst guess rC)) rC a2 a3 (x + 1) 
    where
        addSymbol :: [(String, String)] -> Char -> Int -> [(String, String)]
        addSymbol [] innerGuess _ = [([innerGuess], "0")]
        addSymbol (y:ys) innerGuess 6 = [(fst y ++ [innerGuess], snd y ++ "0")]
        addSymbol (y:ys) innerGuess innerRC = [y] ++ addSymbol ys innerGuess (innerRC + 1)

delStateLet :: GameState -> GameState
delStateLet state@(GameState _ EmptyHistory _ _ _ _) = state
delStateLet state@(GameState _ _ _ _ _ 0) = state
delStateLet state@(GameState c1 c2 6 c3 c4 1) = GameState c1 EmptyHistory 6 c3 c4 0
delStateLet (GameState a1 (History lst) rC a3 a4 wLen) = GameState a1 (History (cleanHistory lst rC wLen)) rC a3 a4 (wLen - 1) where
    cleanHistory :: [(String, String)] -> Int -> Int -> [(String, String)]
    cleanHistory (x:xs) 6 innerWLen = if innerWLen > 1 then [(newNodeLst (fst x) innerWLen, newNodeLst (snd x) innerWLen)] else [] where
        newNodeLst :: String -> Int -> String
        newNodeLst (y:ys) 1 = []
        newNodeLst (y:ys) pos = [y] ++ newNodeLst ys (pos - 1)
    cleanHistory (x:xs) innerRC innerWLen = [x] ++ cleanHistory xs (innerRC + 1) innerWLen

compareWordInput :: String -> UserWord -> CodeAnswer -- Получаем код похожести слова
compareWordInput word input = putYellow word input codeGreenGray codeGreenGray where
    codeGreenGray = innerCompare word word input
    innerCompare :: String -> String -> UserWord -> CodeAnswer
    innerCompare _ [] [] = []
    innerCompare w (x:xs) (y:ys) | x == y = ['2'] ++ innerCompare w xs ys
                                 | otherwise = ['0'] ++ innerCompare w xs ys where
    putYellow :: String -> String -> String -> String -> String
    putYellow _ _ [] _ = []
    putYellow _ [] _ _ = []
    putYellow w (y:ys) (z:zs) c | z == '2' = ['2'] ++ (putYellow w ys zs c)
                                | checkGrayPos w y c = ['1'] ++ (putYellow w ys zs c)
                                | otherwise = ['0'] ++ (putYellow w ys zs c) where
                                    checkGrayPos :: String -> Char -> String -> Bool
                                    checkGrayPos [] _ _ = False
                                    checkGrayPos _ _ [] = False
                                    checkGrayPos (k:ks) y (l:ls) | y == k && l == '0' = True
                                                                | otherwise = checkGrayPos ks y ls 
 
checkWordState :: GameState -> GameState
checkWordState (GameState word (History lst) rC _ _ wLen) = case compareWordInput word (getLastInput lst rC) of
    "22222" -> GameState word (History lst) rC True False wLen
    x -> if rC == 1 then (GameState word (History lst) rC False True wLen) else (GameState word (History (addCode lst rC x)) (rC - 1) False False 0)
    where    
        getLastInput :: [(String, String)] -> Int -> String
        getLastInput (y:ys) 6 = fst y
        getLastInput (y:ys) pos = getLastInput (ys) (pos + 1)
        addCode :: [(String, String)] -> Int -> CodeAnswer -> [(String, String)]
        addCode (y:ys) 6 code = [(fst y, code)]
        addCode (y:ys) pos code =  [y] ++ addCode ys (pos + 1) code

handleInput :: Event -> GameState -> GameState
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state@(GameState _ _ _ _ _ 5) = checkWordState state
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state = state
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state@(GameState _ _ _ _ _ 0) = state
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state = delStateLet state
handleInput (EventKey (Char guess) Down _ _) state@(GameState _ _ _ _ _ 5) = state
handleInput (EventKey (Char guess) Down _ _) state = upgradeStateLet state guess
handleInput _ state = state

updateState :: Float -> GameState -> GameState
updateState _ state = state

----------------------------------PREPROCESSING----------------------------------------------------------------------------

readDictionary :: FileName -> IO (Dictionary String) -- Читаем словарь
readDictionary path = do
    contents <- readFile path
    let ourDictionary = Dictionary $ splitOn " " contents
    let ourCleanDictionary = cleanDictionary ourDictionary
    let ourUpperDictionary = upDictionary ourCleanDictionary
    return $ ourUpperDictionary
    where
        cleanDictionary :: Dictionary String -> Dictionary String -- Очищаем словарь от пустых слов
        cleanDictionary EmptyDictionary = EmptyDictionary
        cleanDictionary (Dictionary []) = Dictionary []
        cleanDictionary (Dictionary (x:xs)) = Dictionary $ cleanList (x:xs) where
            cleanList :: [String] -> [String]
            cleanList [] = []
            cleanList (y:ys) | (length y) == wordLength = y : cleanList ys
                             | otherwise = cleanList ys
        
        upDictionary :: Dictionary String -> Dictionary String
        upDictionary EmptyDictionary = EmptyDictionary
        upDictionary (Dictionary []) = Dictionary []
        upDictionary (Dictionary lst) = Dictionary $ upList lst where
            upList :: [String] -> [String]
            upList [] = []
            upList (x:xs) = (map toLower x) : upList xs 

getRandomIndex :: Dictionary [a] -> IO (Int) -- Получаем рандомный индекс для слова из словаря
getRandomIndex ourDictionary = do
    index <- randomRIO (0, (getDictionaryLength ourDictionary - 1))
    return $ index
    where
        getDictionaryLength :: Dictionary [a] -> Int -- Получаем длину словаря
        getDictionaryLength EmptyDictionary = 0
        getDictionaryLength (Dictionary []) = 0
        getDictionaryLength (Dictionary (x:xs)) = length (x:xs)

randomWord :: Dictionary String -> Int -> Either Int UserWord -- Генерируем случайное слово для отгадывания
randomWord ourDictionary posWord = case ourDictionary of
    EmptyDictionary -> Left $ -1
    Dictionary [] -> Left $ -1
    Dictionary (x:xs) -> if length (x:xs) < minLenDict then Left $ -1 else getWord (x:xs) posWord 0
    where
        getWord :: [String] -> Int -> Int -> Either Int UserWord
        getWord [] _ _ = Left $ -1
        getWord (x:xs) findWordPos curPos | findWordPos == curPos = Right x
                                          | otherwise = getWord xs findWordPos (curPos + 1)

checkGuessWord :: Either Int UserWord -> IO () -- Проверяем безошибочность нашего словаря
checkGuessWord x = case (checkEither x) of
    0 -> anounceError "There are some problems with Dictionary!!!"
    _ -> anounceJust "Everything is okay"
    where 
        checkEither :: Either Int UserWord -> Int
        checkEither (Left _) = 0
        checkEither (Right _) = roundCounts
        anounceError :: String -> IO ()
        anounceError localError = do
            putStrLn localError
        anounceJust :: String -> IO ()
        anounceJust local = do
            putStrLn local

playGame :: Either Int UserWord -> IO () -- Запускаем игру
playGame guessWord = do
    case guessWord of
        (Left _) -> putStrLn "Can't start game!!!"
        (Right w) -> play screen background stepsPerSecond (initialState w) drawState handleInput updateState

startGame :: IO () -- Запуск нашей игры
startGame = do
    putStrLn "Welcome to the world-wide famous game Worldle"

    putStrLn "Local guide:"
    putStrLn "---white(0) means there is no such letter in word"
    putStrLn "---yellow(1) means correct letter for this word but not in correct position"
    putStrLn "---green(2) means correct letter"
    putStrLn "For each symbol in your word you will get combintaion"
    putStrLn "Example:"
    putStrLn "20120 = 2 greens, 2 whites, 1 yellow"

    putStrLn "Please, input file name with same length(6) words to guess"
    inputFile <- getLine
    
    ourDictionary <- readDictionary inputFile
    wordIndex <- getRandomIndex ourDictionary
    let guessWord = randomWord ourDictionary wordIndex
    checkGuessWord guessWord
    playGame guessWord
