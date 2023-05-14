module Main where

import A1
import A2
import A3
import A4
import A5

main :: IO ()
--main = putStrLn "Welcome to Part I of EMURGO Academy's Haskell course!"

{-
main = let doplay = play _EMPTY_BOARD_
    in firstPlayer >>= doplay
-}

main = let doplay = playDo _EMPTY_BOARD_
    in firstPlayer >>= doplay    
