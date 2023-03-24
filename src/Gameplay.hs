module Gameplay where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Constants
import Types

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
    getColor :: Char -> Color -- Получение цвета
    getColor '0' = greyN 0.5
    getColor '1' = yellow
    getColor '2' = green

drawLetters :: String -> Int -> [Picture] -- Отрисовка ввода пользователя
drawLetters [] pos = []
drawLetters (x:xs) pos = [cureLetter x] ++ drawLetters xs (pos + 1) where
    cureLetter :: Char -> Picture -- Отрисовка буквы
    cureLetter x = color black $ scale 0.8 0.7 $ translate xShift yShift $ text $ cleanChar $ show x where
        xShift = fromIntegral $ maxWidthLet + stepLetWidth * (snd $ divMod pos wordLength)
        yShift = fromIntegral $ maxHeightLet - stepLetHeight * (fst $ divMod pos wordLength)
        cleanChar :: [Char] -> [Char] -- Очистка вывода
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
        revealListCode :: [(String, String)] -> String -- Раскрытие списка
        revealListCode [] = []
        revealListCode (x:xs) = snd x ++ revealListCode xs
    letters = drawLetters (revealListSymbols guesses) 0 where
        revealListSymbols :: [(String, String)] -> String -- Раскрытие списка
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

upgradeStateLet :: GameState -> Char -> GameState -- Добавить новую букву в историю
upgradeStateLet (GameState a1 EmptyHistory rC a2 a3 x) guess = GameState a1 (History [([guess], "0")]) rC a2 a3 (x + 1)
upgradeStateLet (GameState a1 (History lst) rC a2 a3 x) guess = GameState a1 (History (addSymbol lst guess rC)) rC a2 a3 (x + 1) 
    where
        addSymbol :: [(String, String)] -> Char -> Int -> [(String, String)] -- Добавялем букву в историю
        addSymbol [] innerGuess _ = [([innerGuess], "0")]
        addSymbol (y:ys) innerGuess 6 = [(fst y ++ [innerGuess], snd y ++ "0")]
        addSymbol (y:ys) innerGuess innerRC = [y] ++ addSymbol ys innerGuess (innerRC + 1)

delStateLet :: GameState -> GameState -- Удаляем последнюю введенную букву
delStateLet state@(GameState _ EmptyHistory _ _ _ _) = state
delStateLet state@(GameState _ _ _ _ _ 0) = state
delStateLet state@(GameState c1 c2 6 c3 c4 1) = GameState c1 EmptyHistory 6 c3 c4 0
delStateLet (GameState a1 (History lst) rC a3 a4 wLen) = GameState a1 (History (cleanHistory lst rC wLen)) rC a3 a4 (wLen - 1) where
    cleanHistory :: [(String, String)] -> Int -> Int -> [(String, String)] -- Чистим историю ввода
    cleanHistory (x:xs) 6 innerWLen = if innerWLen > 1 then [(newNodeLst (fst x) innerWLen, newNodeLst (snd x) innerWLen)] else [] where
        newNodeLst :: String -> Int -> String -- Новое слово
        newNodeLst (y:ys) 1 = []
        newNodeLst (y:ys) pos = [y] ++ newNodeLst ys (pos - 1)
    cleanHistory (x:xs) innerRC innerWLen = [x] ++ cleanHistory xs (innerRC + 1) innerWLen

compareWordInput :: String -> UserWord -> CodeAnswer -- Получаем код похожести слова
compareWordInput word input = putYellow word input codeGreenGray codeGreenGray where
    codeGreenGray = innerCompare word word input
    innerCompare :: String -> String -> UserWord -> CodeAnswer -- Внутрення проверка
    innerCompare _ [] [] = []
    innerCompare w (x:xs) (y:ys) | x == y = ['2'] ++ innerCompare w xs ys
                                 | otherwise = ['0'] ++ innerCompare w xs ys where
    putYellow :: String -> String -> String -> String -> String -- Добавляем желтые буквы
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
 
checkWordState :: GameState -> GameState -- Проверяем введенное слово
checkWordState (GameState word (History lst) rC _ _ wLen) = case compareWordInput word (getLastInput lst rC) of
    "22222" -> GameState word (History lst) rC True False wLen
    x -> if rC == 1 then (GameState word (History lst) rC False True wLen) else (GameState word (History (addCode lst rC x)) (rC - 1) False False 0)
    where    
        getLastInput :: [(String, String)] -> Int -> String -- Получаем последене введенное слово
        getLastInput (y:ys) 6 = fst y
        getLastInput (y:ys) pos = getLastInput (ys) (pos + 1)
        addCode :: [(String, String)] -> Int -> CodeAnswer -> [(String, String)] -- Добавляем код для ввода пользователя
        addCode (y:ys) 6 code = [(fst y, code)]
        addCode (y:ys) pos code =  [y] ++ addCode ys (pos + 1) code

handleInput :: Event -> GameState -> GameState -- Обработка ввода пользователя
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state@(GameState _ _ _ _ _ 5) = checkWordState state
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state = state
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state@(GameState _ _ _ _ _ 0) = state
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state = delStateLet state
handleInput (EventKey (Char _) Down _ _) state@(GameState _ _ _ _ _ 5) = state
handleInput (EventKey (Char guess) Down _ _) state = if alphabetHas alphabet guess then upgradeStateLet state guess else state where
    alphabet :: [Char] -- Допустимый ввод
    alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 
        'g', 'h', 'i', 'k', 'l', 'm', 'n', 
        'o', 'p', 'q', 'r', 's', 't', 'v', 
        'x', 'y', 'z']
    alphabetHas :: [Char] -> Char -> Bool -- Проверка допустимости ввода
    alphabetHas [] _ = False
    alphabetHas (x:xs) symbol | symbol == x = True
                              | otherwise = alphabetHas xs symbol 
handleInput _ state = state

updateState :: Float -> GameState -> GameState -- Обновляем состояние посекундно
updateState _ state = state

playGame :: Either Int UserWord -> IO () -- Запускаем игру
playGame guessWord = do
    case guessWord of
        (Left _) -> putStrLn "Can't start game!!!"
        (Right w) -> play screen background stepsPerSecond (initialState w) drawState handleInput updateState