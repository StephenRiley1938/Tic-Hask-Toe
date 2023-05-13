module A4 where

import A1
import A2
import A3 hiding (
  _HEADER_,
  showSquares,
  dropFirstCol,
  dropLastCol,
  formatRows,
  isWinningLine,
  prependRowIndices
  )

-- *** Assignment 4-1 *** --

-- Q#01

_HEADER_ :: String
--_HEADER_ = " " ++ formatLine (showInts _RANGE_)

_HEADER_ = " " ++ formatLine (map show _RANGE_)


-- Q#02

showSquares :: [Square] -> [String]
--showSquares [] = []
--showSquares [a] = [showSquare a]
--showSquares (a:as) = showSquares [a] ++ showSquares as
showSquares sqs = map showSquare sqs

-- Q#03

dropFirstCol :: [Row] -> [Row]
--dropFirstCol (r:rs) = (tail r) : dropFirstCol rs
--dropFirstCol [] = []

dropFirstCol board = map tail board

-- Q#04

dropLastCol :: [Row] -> [Row]
--dropLastCol (r:rs) = (init r) : dropLastCol rs
--dropLastCol [] = []

dropLastCol board = map init board

--Q#05

formatRows :: [Row] -> [String]
--formatRows [] = []
--formatRows [row] = [formatLine (showSquares row)]
--formatRows (row:rows) = formatRows [row] ++ formatRows rows

formatRows rows = map (\r -> formatLine (showSquares r)) rows

-- Q#06

isWinningLine_ :: Player -> Line -> Bool
--isWinningLine player line = go False player line
--  where
--    go :: Bool -> Player -> Line -> Bool
--    go bool _ [] = bool
--    go _ player (p:ps) = p == player && go True player ps
isWinningLine_ player line = if null line
  then False
  else if (null (filter (/= player) line)) then True
  else False

-- *** Assignment 4-2 *** --

-- Q#07

isWinningLine :: Player -> Line -> Bool

isWinningLine player line = if null line
  then False
  else foldr (\p r -> r && p == player) True line

-- Q#08

_X_WIN_ = [ [X, O, O]
          , [O, X, O]
          , [O, O, X]
          ]

_O_WIN_ = [ [O, X, O]
          , [X, X, O]
          , [X, O, O]
          ]

hasWon :: Player -> Board -> Bool
hasWon player board = foldr (\line r -> r || isWinningLine player line) False (getAllLines board) 

-- Q#09
{-
data GameState = Xwon | Owon | TieGame | GameInProgress deriving (Show, Eq)
Xwon = Xwon
Owon = Owon
TieGame = TieGame
GameInProgress = GameInProgress
-}
getGameState :: Board -> GameState
getGameState board
  | hasWon X board = Xwon
  | hasWon O board = Owon
  | isTied board = TieGame
  | otherwise = GameInProgress

playMove :: Player -> Board -> Move -> (GameState, Board)
playMove player board move = let newboard = putSquare player board move
  in (getGameState newboard, newboard)

-- Q#10
{-
prependRowIndices :: [String] -> [String]
prependRowIndices ss = go (indexRowStrings ss)
  where
    go :: [(Char, String)] -> [String]
    go [] = []
    go ((c,s):ps) = (c : s) : go ps
    -}

prependRowIndices :: [String] -> [String]
prependRowIndices ss = zipWith (:) ['A' ..] ss

-- Q#11
 
formatBoard :: Board -> String
--formatBoard board = (unlines (_HEADER_ : (prependRowIndices (formatRows board))))

formatBoard board = unlines $ _HEADER_ : (prependRowIndices . formatRows $ board)