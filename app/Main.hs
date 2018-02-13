module Main where

import Lib
import Boardvector
import MoveSelectMechanical
import MoveSelectTree
import System.IO
import Evolution

import Control.Monad.Random
import System.Random.Mersenne.Pure64

main :: IO ()
-- output is written immediately, it's never stored in the buffer
main = do
    g <- newPureMT
    let (p,g') = runRand (randomGenomes 2 64 (0.1::Double) (1.0::Double) ) g
    let gen1 =  head p
    let gen2 = head $ tail p
    hSetBuffering stdout NoBuffering
    -- using random moves
    -- play aiNextState aiNextState (GameState initialBoard White)
    play gen1 gen2 performMoveAIalphabeta6Ply performMoveAIalphabeta0Ply (GameState initialBoard Black)
