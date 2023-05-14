module A5 where

import A1
import A2
import A3
import A4

import System.Random.Stateful (globalStdGen, uniformM)
import Control.Monad (when)

-- *** Assignment 5-1 *** --

-- Q#01
printBoard :: Board -> IO ()
printBoard board = putStrLn (formatBoard board)

-- Q#02
_LOGO_PATH_ :: FilePath
_LOGO_PATH_ = "./assets/logo.txt"

printLogo :: IO ()
printLogo = readFile _LOGO_PATH_ >>= putStrLn

-- Q#03
_RANDOM_BOOL_ :: IO Bool
_RANDOM_BOOL_ = uniformM globalStdGen

firstPlayer :: IO Player
firstPlayer = _RANDOM_BOOL_ >>= (\p -> (return . getFirstPlayer) p)

-- Q#04

getMove :: Board -> IO Move
getMove board = getLine >>= (\line -> let move = stringToMove line
    in  if (move == _INVALID_MOVE_) || not (isValidMove board move)
            then putStrLn "Invalid move! Try again" >> getMove board
        else
            return move)

-- Q#05

play :: Board -> Player -> IO ()
play board player =
    when _DISPLAY_LOGO_ printLogo >>
    printBoard board >>
    print (promptPlayer player) >>
    getMove board >>= (\move -> let (gamestate, newboard) = playMove player board move
        in  if gamestate == GameInProgress
                then play newboard (switchPlayer player)
            else
                printBoard newboard >>
                putStrLn (showGameState gamestate) >>
                return ()
            )



-- *** Assignment 5-2 *** --

-- Q#07

printLogoDo :: IO ()
printLogoDo = do
    lf <- readFile _LOGO_PATH_
    putStrLn lf

-- Q#08

firstPlayerDo :: IO Player
firstPlayerDo = do
    rb <- _RANDOM_BOOL_
    (return . getFirstPlayer) rb

-- Q#09

getMoveDo :: Board -> IO Move
getMoveDo board = do
    line <- getLine
    let move = stringToMove line
    if (move == _INVALID_MOVE_) || not (isValidMove board move)
        then do
            putStrLn "Invalid move! Try again"
            getMove board
        else do
            return move

-- Q#10

playDo :: Board -> Player -> IO ()
playDo board player = do
    when _DISPLAY_LOGO_ printLogo
    printBoard board
    print (promptPlayer player)
    move <- getMove board
    let (gamestate, newboard) = playMove player board move
    if gamestate == GameInProgress
        then do
            play newboard (switchPlayer player)
        else do
            printBoard newboard
            putStrLn (showGameState gamestate)
            return ()
            