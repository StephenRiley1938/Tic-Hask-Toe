module A3 where

import A1
import A2

import Data.List (transpose)

-- *** Assignment 3-1 ***

-- Q#01 

showInts :: [Int] -> [String]
--showInts [] = error "ShowInts called on and empty list."
showInts [] = []
showInts [a] = [(show a)]
--showInts (a:as) = [(show a)] ++ showInts as
showInts (a:as) = showInts [a] ++ showInts as

_HEADER_ :: String
_HEADER_ = " " ++ formatLine (showInts _RANGE_)

-- Q#02

showSquares :: [Square] -> [String]
showSquares [] = []
showSquares [a] = [showSquare a]
--showSquares (a:as) = [showSquare a] ++ showSquares as
showSquares (a:as) = showSquares [a] ++ showSquares as

-- Q#03

formatRows :: [Row] -> [String]
formatRows [] = []
formatRows [row] = [formatLine (showSquares row)]
--formatRows (row:rows) = [formatLine (showSquares row)] ++ formatRows rows
formatRows (row:rows) = formatRows [row] ++ formatRows rows


-- Q#04

isColEmpty :: Row -> Int -> Bool
isColEmpty (r:rs) c = (r == E && c == 0) || isColEmpty rs (c - 1)
isColEmpty [] _ = False

-- Q#05

dropFirstSquare :: [Square] -> [Square]
dropFirstSquare (s:ss) = ss
dropFirstSquare [] = []

dropLastSquare :: [Square] -> [Square]
dropLastSquare (s:ss) = init (s:ss)
dropLastSquare [] = []

dropFirstCol :: [Row] -> [Row]
dropFirstCol (r:rs) = (tail r) : dropFirstCol rs
dropFirstCol [] = []

dropLastCol :: [Row] -> [Row]
dropLastCol (r:rs) = (init r) : dropLastCol rs
dropLastCol [] = []

-- Q#06

getDiag1 :: Board -> Line
getDiag1 (r:rs) = head r : getDiag1 (dropFirstCol rs)
getDiag1 [] = []

getDiag2 :: Board -> Line
getDiag2 (r:rs) = last r : getDiag2 (dropLastCol rs)
getDiag2 [] = []

getAllLines :: Board -> [Line]
--getAllLines board = concat [board, transpose board, [getDiag1 board], [getDiag2 board]]
getAllLines board = concat [board, transpose board, [getDiag1 board, getDiag2 board]]

-- *** Assignment 3-2 ***

-- Q#07

putSquare :: Player -> Board -> Move -> Board
putSquare _ [] _ = []
putSquare player (r:rs) (ri, ci)
  | ri == 0 = replaceSquareInRow player ci r : rs
  | otherwise = r : putSquare player rs (ri - 1, ci)

--replaceSquareInRow :: Player -> Int -> Row -> Row

-- Q#08

prependRowIndices :: [String] -> [String]
prependRowIndices ss = go [] (indexRowStrings ss)
  where
    go :: [String] -> [(Char, String)] -> [String]
    go acc [] = reverse acc
    go acc ((c,s):ps) = go ((c : s) : acc) ps

--indexRowStrings :: [String] -> [(Char, String)]

-- Q#09

isWinningLine :: Player -> Line -> Bool
isWinningLine player line = go False player line
  where
    go :: Bool -> Player -> Line -> Bool
    go bool _ [] = bool
    go _ player (p:ps) = p == player && go True player ps

-- Q#10

isValidMove :: Board -> Move -> Bool
isValidMove board move
--  | null board = False
  | not (isMoveInBounds move) = False
  | otherwise = go board move
  where
    go :: Board -> Move -> Bool
    go (row:rows) (ri, ci)
      | ri == 0   = isColEmpty row ci
      | otherwise = go rows (ri - 1, ci)
    go [] _ = False


--isColEmpty :: Row -> Int -> Bool

{-
putSquare player (r:rs) (ri, ci)
  | ri == 0 = (replaceSquareInRow player ci r) : rs
  | otherwise = r : putSquare player rs (ri - 1, ci)

-}

