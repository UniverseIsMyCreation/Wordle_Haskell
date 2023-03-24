module Constants where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

import Types

minLenDict :: Int -- Минимальная длина словаря
minLenDict = 7

wordLength :: Int -- Длина слова
wordLength = 5

roundCounts :: Int -- Количество раундов
roundCounts = 6

cellWidth :: Float -- Ширина клетки
cellWidth = 100

cellHeight :: Float -- Высота клетки
cellHeight = 100

maxHeight :: Int -- Начальное положение отрисовки
maxHeight = 300

maxWidth :: Int -- Начальное положение отрисовки
maxWidth = -220

step :: Int -- Шаг для отрисовки клетки
step = 100

maxWidthLet :: Int -- Начальное положение отрисовки
maxWidthLet = -320

maxHeightLet :: Int -- Начальное положение отрисовки
maxHeightLet = 390

stepLetWidth :: Int -- шаг между буквами в ширину
stepLetWidth = 122

stepLetHeight :: Int -- шаг между буквами в высоту
stepLetHeight = 140

initialState :: String -> GameState -- Начальное состояние
initialState word = GameState word EmptyHistory roundCounts False False 0

background :: Color -- Фон
background = white

screen :: Display -- Экран
screen = InWindow "Wordle" (1600, 800) (40, 40)

stepsPerSecond :: Int -- Для обновление состояния
stepsPerSecond = 1