module Gameplay(
    drawBoard,
    drawLetters,
    drawState,
    upgradeStateLet,
    delStateLet,
    checkWordState,
    compareWordInput,
    handleInput,
    updateState,
    playGame
) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Constants
import Types

----------------------------------DRAW STATE----------------------------------------------------------------------------

drawBoard :: String -> Int -> [Picture] -- Create game field
drawBoard [] x = if x == maxLetters
                 then [] 
                 else boardState where
    boardState = [figureSolid] ++ [figureWire] ++ (drawBoard [] $ x + 1)
    figureSolid = color white $ translate xShift yShift $ rectangleSolid cellWidth cellHeight
    figureWire = translate xShift yShift $ rectangleWire cellWidth cellHeight
    xShift = fromIntegral $ maxWidth + step * (snd $ divMod x wordLength)
    yShift = fromIntegral $ maxHeight - step * (fst $ divMod x wordLength)

drawBoard (y:ys) x = if x == maxLetters
                     then []
                     else boardState where
    boardState = [color (getColor y) $ figure] ++ (drawBoard ys $ x + 1)
    figure = translate xShift yShift $ rectangleSolid cellWidth cellHeight
    xShift = fromIntegral $ maxWidth + step * (snd $ divMod x wordLength)
    yShift = fromIntegral $ maxHeight - step * (fst $ divMod x wordLength)
    getColor :: Char -> Color -- Get color
    getColor code_symbol = if code_symbol == greyCode 
                           then greyN greyScale
                           else if code_symbol == yellowCode 
                           then yellow
                           else green

drawLetters :: String -> Int -> [Picture] -- Draw user's input
drawLetters [] _ = []
drawLetters (x:xs) pos = [cureLetter x] ++ drawLetters xs (pos + 1) where
    cureLetter :: Char -> Picture -- Draw letter
    cureLetter letter = curPicture where
        curPicture = color black $ scale scaleLetterW scaleLetterH $ translate xShift yShift $ cleanText
        cleanText = text $ cleanChar $ show letter
        xShift = fromIntegral $ maxWidthLet + stepLetWidth * (snd $ divMod pos wordLength)
        yShift = fromIntegral $ maxHeightLet - stepLetHeight * (fst $ divMod pos wordLength)
        cleanChar :: [Char] -> [Char] -- Clean output
        cleanChar [] = []
        cleanChar (y:ys) | y == '\'' = cleanChar ys
                         | otherwise = y : cleanChar ys

drawState :: GameState -> Picture -- Current gamestate
drawState (GameState _ _ _ True False _) = pictures gameCondition where
    gameCondition = [winPic]
    winPic = scale scaleStateW scaleStateH $ translate (-700) 0 $ winText
    winText = text $ "Congratulate, you win!!!"

drawState (GameState guessWord _ _ False True _) = pictures $ gameCondition where
    gameCondition = [losePic, correctAnswer]
    losePic = scale scaleStateW scaleStateH $ translate (-700) 200 $ loseText
    loseText = text $ "You lose!!!"
    correctAnswer = scale scaleStateW scaleStateH $ translate (-700) (-200) $ correctWordText
    correctWordText = text $ "Correct word was: " ++ show guessWord

drawState (GameState _ EmptyHistory curAttemtps _ _ _) = pictures $ gameCondition where
    gameCondition = frames ++ [remainingPic, rules1, rules2, press11, press12, press2, press3]
    frames = drawBoard [] 0
    remainingPic = scale scaleStateW scaleStateH $ translate (-700) (-800) $ remainingText
    remainingText = text $ "Guesses remaining: " ++ show curAttemtps
    rules1 = scale scaleRuleW scaleRuleH $ translate (-2500) (1000) $ instructionText
    instructionText = text $ "Here's instruction"
    rules2 = scale scaleRuleW scaleRuleH $ translate (-2500) (800) $ gameText
    gameText = text $ "for this game:"
    press11 = scale scaleRuleW scaleRuleH $ translate (-2500) (600) $ enterText
    enterText = text $ "1) Enter-enter"
    press12 = scale scaleRuleW scaleRuleH $ translate (-2500) (450) $ letterText
    letterText = text $ "the 5 letter word"
    press2 = scale scaleRuleW scaleRuleH $ translate (-2500) (250) $ spaceText
    spaceText = text $ "2) Space-cancel letter"
    press3 = scale scaleRuleW scaleRuleH $ translate (-2500) (50) $ inputText
    inputText = text $ "3) Letters-input letter"

drawState (GameState _ (History curGuesses) curAttemtps _ _ _) = pictures $ gameCondition where
    gameCondition = frames ++ letters ++ allRules
    frames = drawBoard (revealListCode curGuesses) 0 where
        revealListCode :: [(String, String)] -> String -- Reveal list
        revealListCode [] = []
        revealListCode (x:xs) = snd x ++ revealListCode xs
    letters = drawLetters (revealListSymbols curGuesses) 0 where
        revealListSymbols :: [(String, String)] -> String -- Reveal list
        revealListSymbols [] = []
        revealListSymbols (x:xs) = fst x ++ revealListSymbols xs
    allRules = [remainingPic, rules1, rules2, press11, press12, press2, press3]
    remainingPic = scale scaleStateW scaleStateH $ translate (-700) (-800) $ remainingText
    remainingText = text $ "Guesses remaining: " ++ show curAttemtps
    rules1 = scale scaleRuleW scaleRuleH $ translate (-2500) (1000) $ instructionText
    instructionText = text $ "Here's instruction"
    rules2 = scale scaleRuleW scaleRuleH $ translate (-2500) (800) $ gameText
    gameText = text $ "for this game:"
    press11 = scale scaleRuleW scaleRuleH $ translate (-2500) (600) $ enterText
    enterText = text $ "1) Enter-enter"
    press12 = scale scaleRuleW scaleRuleH $ translate (-2500) (450) $ letterText
    letterText = text $ "the 5 letter word"
    press2 = scale scaleRuleW scaleRuleH $ translate (-2500) (250) $ spaceText
    spaceText = text $ "2) Space-cancel letter"
    press3 = scale scaleRuleW scaleRuleH $ translate (-2500) (50) $ inputText
    inputText = text $ "3) Letters-input letter"

----------------------------------HANDLE USER ACTIONS----------------------------------------------------------------------------

upgradeStateLet :: GameState -> Char -> GameState -- Add letter to the history
upgradeStateLet (GameState a1 EmptyHistory rC a2 a3 x) guess = newGameStateFirst where
    newGameStateFirst = GameState a1 (History [([guess], [greyCode])]) rC a2 a3 (x + 1)
upgradeStateLet (GameState a1 (History lst) rC a2 a3 x) guess = newGameStateSecond where
    newGameStateSecond = GameState a1 (History (addSymbol lst guess rC)) rC a2 a3 (x + 1)
    addSymbol :: [(String, String)] -> Char -> Int -> [(String, String)]
    addSymbol [] innerGuess _ = [([innerGuess], [greyCode])]
    addSymbol (y:_) innerGuess 6 = [(fst y ++ [innerGuess], snd y ++ [greyCode])]
    addSymbol (y:ys) innerGuess innerRC = [y] ++ addSymbol ys innerGuess (innerRC + 1)

delStateLet :: GameState -> GameState -- Delete last input letter
delStateLet state@(GameState _ EmptyHistory _ _ _ _) = state
delStateLet state@(GameState _ _ _ _ _ 0) = state
delStateLet (GameState c1 _ 6 c3 c4 1) = newGameState where
    newGameState = GameState c1 EmptyHistory 6 c3 c4 0
delStateLet (GameState a1 (History lst) rC a3 a4 wLen) = newGameState where
    newGameState = GameState a1 (History (cleanHistory lst rC wLen)) rC a3 a4 (wLen - 1)
    cleanHistory :: [(String, String)] -> Int -> Int -> [(String, String)] -- Clean input history letter
    cleanHistory curHistoryInput 6 innerWLen = if innerWLen > 1 
                                     then [(kFirstLet, kFirstNum)] 
                                     else [] where
        kFirstLet = take (innerWLen - 1) (fst . head $ curHistoryInput)
        kFirstNum = take (innerWLen - 1) (snd . head $ curHistoryInput)
    cleanHistory (x:xs) innerRC innerWLen = [x] ++ cleanHistory xs (innerRC + 1) innerWLen
    cleanHistory [] _ _ = [] -- excessive pattern


compareWordInput :: String -> UserWord -> CodeAnswer -- Get similarity code for word
compareWordInput guessWord input = putYellow guessWord input codeGreenGray codeGreenGray where
    codeGreenGray = innerCompare guessWord input
    innerCompare :: String -> UserWord -> CodeAnswer -- Inner check
    innerCompare [] _ = []
    innerCompare _ [] = []
    innerCompare (x:xs) (y:ys) | x == y = [greenCode] ++ innerCompare xs ys
                               | otherwise = [greyCode] ++ innerCompare xs ys
    putYellow :: String -> String -> String -> String -> String -- Add yellow letter
    putYellow _ _ [] _ = []
    putYellow _ [] _ _ = []
    putYellow w (y:ys) (z:zs) c | z == greenCode = [greenCode] ++ (putYellow w ys zs c)
                                | checkGrayPos w y c = [yellowCode] ++ (putYellow w ys zs c)
                                | otherwise = [greyCode] ++ (putYellow w ys zs c) where
                                    checkGrayPos :: String -> Char -> String -> Bool
                                    checkGrayPos [] _ _ = False
                                    checkGrayPos _ _ [] = False
                                    checkGrayPos (k:ks) curIn (l:ls) | curIn == k && l == greyCode = True
                                                                     | otherwise = checkGrayPos ks y ls

checkWordState :: GameState -> GameState -- Check input word
checkWordState state@(GameState _ EmptyHistory _ _ _ _) = state -- excessive pattern
checkWordState (GameState guessWord (History lst) rC _ _ wLen) 
    | returnCode == rightWordCode = (GameState guessWord (History lst) rC True False wLen)
    | rC == 1 = (GameState guessWord (History lst) rC False True wLen) 
    | otherwise = (GameState guessWord (History (addCode lst rC returnCode)) (rC - 1) False False 0) where
        returnCode = compareWordInput guessWord (getLastInput lst rC)
        getLastInput :: [(String, String)] -> Int -> String -- Get last input word
        getLastInput [] _ = [] -- excessive pattern
        getLastInput historyPoints 6 = fst . head $ historyPoints
        getLastInput historyPoints pos = getLastInput (tail historyPoints) (pos + 1)
        addCode :: [(String, String)] -> Int -> CodeAnswer -> [(String, String)] -- Add code for user's input
        addCode [] _ _ = [] -- excessive pattern
        addCode historyPoint 6 code = [(fst . head $ historyPoint, code)]
        addCode (y:ys) pos code =  [y] ++ addCode ys (pos + 1) code

handleInput :: Event -> GameState -> GameState -- Procces input of player
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state@(GameState _ _ _ _ _ 5) = checkWordState state
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) state = state
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state@(GameState _ _ _ _ _ 0) = state
handleInput (EventKey (SpecialKey KeySpace) Down _ _) state = delStateLet state
handleInput (EventKey (Char _) Down _ _) state@(GameState _ _ _ _ _ 5) = state
handleInput (EventKey (Char guess) Down _ _) state = if alphabetHas alphabet guess 
                                                     then upgradeStateLet state guess 
                                                     else state where
    alphabetHas :: [Char] -> Char -> Bool -- Chech input letter
    alphabetHas [] _ = False
    alphabetHas (x:xs) symbol | symbol == x = True
                              | otherwise = alphabetHas xs symbol 
handleInput _ state = state

updateState :: Float -> GameState -> GameState -- Update the gamestate each second
updateState _ state = state

playGame :: Either Int UserWord -> IO () -- Launch the game and draw the gamestate
playGame guessWord = do
    case guessWord of
        (Left _) -> putStrLn "Can't start game!!!"
        (Right w) -> play screen background stepsPerSecond (initialState w) drawState handleInput updateState