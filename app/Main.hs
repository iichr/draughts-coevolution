module Main where

import Lib
import Boardvector
import MoveSelectMechanical
import MoveSelectTree
import System.IO

main :: IO ()
-- output is written immediately, it's never stored in the buffer
main = do
    hSetBuffering stdout NoBuffering
    -- using random moves
    -- play aiNextState aiNextState (GameState initialBoard White)
    play aiNextState aiNextState (GameState initialBoard White)
