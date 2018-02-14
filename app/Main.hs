module Main where

import Lib
import Boardvector
import MoveSelectMechanical
import MoveSelectTree
import System.IO
import Evolution

import Control.Monad.Random
import System.Random.Mersenne.Pure64


-- EVALUATION (fitness)
-- given a population of genomes P
-- generate 100 fixed oponents, keep them
-- play EACH individual from p
-- count how many times each individual won
-- zip that with the genome in the form of (genome, number of wins)
-- at the end map to obtain a list of [(genome, number of wins)]
evaluate :: Genome Double -> [Genome Double] -> (Genome Double, Int)
evaluate genome opponents = (genome, singleFitness) 
    where
        singleGenomeScores = [score | o <- opponents, let score = playnonIO 150 o genome ai ai gs]
        ai = performMoveAIalphabeta4PlyNonIO
        gs = (GameState initialBoard Black)
        singleFitness = length (filter (==(-1)) singleGenomeScores)


main :: IO ()
-- output is written immediately (never stored in the buffer)
main = do
    g <- newPureMT
    let (p,g') = runRand (randomGenomes 2 64 (0.1::Double) (1.0::Double) ) g
    let gen1 = take 64 $ repeat (0.5::Double)
    let gen2 = head p
    --head $ tail p
    --take 64 $ repeat (1.0::Double)
    hSetBuffering stdout NoBuffering
    -- using random moves
    -- play aiNextState aiNextState (GameState initialBoard White)
    --play 150 gen1 gen2 performMoveAIalphabeta3Ply performMoveAIalphabeta0Ply (GameState initialBoard Black)
    --let run = playnonIO 150 gen1 gen2 performMoveAIalphabeta5PlyNonIO performMoveAIalphabeta5PlyNonIO (GameState initialBoard Black)
    
    let hundredOpponents = evalRand (randomGenomes 100 64 (0.1::Double) (1.0::Double) ) g
    let evalsingletest = evaluate gen2 hundredOpponents
    print evalsingletest
    --print run
    -- ran 10 times with gen2 set to ones - 6 B 1 W 5 Draw