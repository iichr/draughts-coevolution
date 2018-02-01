module Main where

import Lib
import Boardvector
import System.IO

main :: IO ()
-- output is written immediately, it's never stored in the buffer
main = do
    hSetBuffering stdout NoBuffering
    play aiNextState aiNextState (GameState initialBoard White)
