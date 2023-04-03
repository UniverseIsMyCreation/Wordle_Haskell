module Constants(
    minLenDict,
    wordLength,
    roundCounts,
    cellWidth,
    cellHeight,
    maxHeight,
    maxWidth,
    step,
    maxWidthLet,
    maxHeightLet,
    stepLetWidth,
    stepLetHeight,
    initialState,
    background,
    screen,
    stepsPerSecond,
    greenCode,
    greyCode,
    yellowCode,
    greyScale,
    alphabet,
    scaleLetterW,
    scaleLetterH,
    scaleStateW,
    scaleStateH,
    scaleRuleW,
    scaleRuleH,
    rightWordCode,
    maxLetters
) where

import Graphics.Gloss

import Types

minLenDict :: Int -- Minimum dictionary length
minLenDict = 7

wordLength :: Int -- Word length
wordLength = 5

roundCounts :: Int -- Round counts
roundCounts = 6

cellWidth :: Float -- Cell width
cellWidth = 100

cellHeight :: Float -- Cell height
cellHeight = 100

maxHeight :: Int -- Initial position to draw (height)
maxHeight = 300

maxWidth :: Int -- Initial position to draw (width)
maxWidth = -220

step :: Int -- Step to draw cell
step = 100

maxWidthLet :: Int -- Initial position to draw (width) (letter)
maxWidthLet = -320

maxHeightLet :: Int -- Initial position to draw (height) (letter)
maxHeightLet = 390

stepLetWidth :: Int -- Step to draw letter (width)
stepLetWidth = 122

stepLetHeight :: Int -- Step to draw letter (height)
stepLetHeight = 140

initialState :: String -> GameState -- Initial gamestate
initialState word2guess = GameState word2guess EmptyHistory roundCounts False False 0

background :: Color -- Background
background = white

screen :: Display -- Screen
screen = InWindow "Wordle" (1600, 800) (40, 40)

stepsPerSecond :: Int -- to update the gamestate
stepsPerSecond = 1

greenCode :: Char -- Code for right letter
greenCode = '2'

yellowCode :: Char -- Code for right letter but other position
yellowCode = '1'

greyCode :: Char -- There is not such letter
greyCode = '0'

greyScale :: Float -- Scale for grey color
greyScale = 0.5

alphabet :: [Char] -- Available letters
alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 
        'g', 'h', 'i', 'k', 'l', 'm', 'n', 
        'o', 'p', 'q', 'r', 's', 't', 'v', 
        'x', 'y', 'z', 'u', 'w']

scaleLetterW :: Float
scaleLetterW = 0.8

scaleLetterH :: Float
scaleLetterH = 0.7

scaleStateW :: Float
scaleStateW = 0.6

scaleStateH :: Float
scaleStateH = 0.4

scaleRuleW :: Float
scaleRuleW = 0.3

scaleRuleH :: Float
scaleRuleH = 0.3

rightWordCode :: String
rightWordCode = "22222"

maxLetters :: Int
maxLetters = 30