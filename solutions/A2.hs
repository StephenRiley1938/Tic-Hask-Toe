{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module A2 where

import A1
import Data.List (intercalate)

-- *** Assignment 2-1 *** --

-- Q#01
promptPlayer :: Player -> String
promptPlayer p = concat ["Player ", showSquare p, "'s turn: enter a row and column position (ex. A1)"]

-- Q#02

_RANGE_ :: [Int]
_RANGE_ = [0 .. (_SIZE_ - 1)]

-- Q#03

isDigit :: Char -> Bool
isDigit c
  | elem c digits = True
  | otherwise = False
  where digits = ['0' .. '9']

readDigit :: Char -> Int
readDigit c
  | isDigit c = read [c] :: Int
  | otherwise = -1

-- Q#04

_EMPTY_ROW_ = replicate _SIZE_ Empty

_EMPTY_BOARD_ = replicate _SIZE_ _EMPTY_ROW_

-- Q#05

isTied :: Board -> Bool
isTied board
  | elem Empty (concat board) = False
  | otherwise = True

_TIED_BOARD_ :: Board
_TIED_BOARD_ = [
                [X, O, O],
                [O, X, X],
                [O, X, O]
               ]

-- Q#06

indexRowStrings :: [String] -> [(Char, String)]
indexRowStrings strings = zip ['A' ..] strings

-- Q#07

formatLine :: [String] -> String
formatLine line = intercalate _SEP_ line

-- *** Assignment 2-2 *** --

-- Q#08

isMoveInBounds :: Move -> Bool
isMoveInBounds (r, c) = all (== True) [elem r bounds, elem c bounds]
  where bounds = [0 .. _SIZE_ - 1]

-- Q#09

stringToMove :: String -> Move
stringToMove [rowLetter, colDigit] =
  let
    row = case rowLetter of
      'a' -> 0
      'A' -> 0
      'b' -> 1
      'B' -> 1
      'c' -> 2
      'C' -> 2
      _   -> -1
    col = readDigit colDigit
    move = (row, col)
  in
    if isMoveInBounds move
    then move
    else _INVALID_MOVE_

stringToMove [] = _INVALID_MOVE_
stringToMove [_] = _INVALID_MOVE_
stringToMove (_:_:_) = _INVALID_MOVE_

-- Q#10

replaceSquareInRow :: Player -> Int -> Row -> Row
replaceSquareInRow play col row = 
  if isMoveInBounds (0, col)
  then
    let
      splt = splitAt col row
      (x, y) = splt
    in
      concat [x, (play : (tail y))]
      --concat [(fst splt), (play : (tail (snd splt)))]
  else row
  
rsX = replaceSquareInRow X

rsO = replaceSquareInRow O
