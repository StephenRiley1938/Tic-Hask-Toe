module A1 where

import Data.Char (toUpper)

-- *** Assignment 1-1 *** --

-- Q#01

_SIZE_ :: Int
_SIZE_ = 3

-- Q#02

_DISPLAY_LOGO_ :: Bool
_DISPLAY_LOGO_ = True

-- Q#03

convertRowIndex :: Char -> Int
convertRowIndex c = (fromEnum (toUpper c)) - 65

-- Q#04

_INVALID_MOVE_ :: (Int, Int)
_INVALID_MOVE_ = (-1, -1)

-- Q#05

_SEP_ :: String
_SEP_ = "_|_"

-- *** Assignment 1-2 *** --

-- Q#06

data Square = X | O | E deriving (Show, Eq)
X = X
O = O
E = E

-- Q#07

data GameState = Xwon | Owon | TieGame | GameInProgress deriving (Show, Eq)
Xwon = Xwon
Owon = Owon
TieGame = TieGame
GameInProgress = GameInProgress


-- Q#08

type Player = Square

type Row = [Square]

type Line = [Square]

type Board = [Row]

type Move = (Int, Int)

-- Q#09

getFirstPlayer :: Bool -> Player
getFirstPlayer flip = if flip then X
                        else O

getFirstPlayer_ :: Bool -> Player                        
getFirstPlayer_ flip
  | flip = X
  | otherwise = O


-- Q#10

showGameState :: GameState -> String
showGameState gs = case gs of
  Xwon    -> "X won"
  Owon    -> "O won"
  TieGame -> "Tie Game"
  _       -> "Game In Progress"

-- Q#11

switchPlayer :: Player -> Player
switchPlayer X = O
switchPlayer O = X
switchPlayer E = E

-- Q#12
showSquare ::  Square -> String
showSquare X = "X"
showSquare O = "O"
showSquare E = "_"



