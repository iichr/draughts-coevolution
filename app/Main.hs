module Main where

import System.IO
import Control.Monad.Random
import System.Random.Mersenne.Pure64
import Data.List
-- for comparisons
import Data.Ord

import Lib
import Utils
import DraughtsBoard
import GamePlay
import MoveSelectTree
import MoveSelectMechanical
import Evolution

-- The output is written immediately (never stored in the buffer)
main :: IO ()
main = do
    g <- newPureMT
    hSetBuffering stdout NoBuffering
    
    -- ***************************************
    -- * SET UP FIXED GENOMES TO TEST AGAINST
    let genOnesOnly = take 32 $ repeat (0.1::Double)
    let genZerosOnly = take 32 $ repeat (0.0::Double)

     -- ***************************************
    -- * SET UP RANDOM POPULATION OF GENOMES TO TEST AGAINST
    let pop64 = evalRand (randomGenomes 100 32 (0.0::Double) (1.0::Double) ) g

    -- let run = playnonIO 150 genOnesOnly gen1 performMoveAIalphabeta3PlyNonIO performMoveAIalphabeta3PlyNonIO (GameState initialBoard Black)
    -- play 150 genOnesOnly gen1 performMoveMinimax3Ply performMoveMinimax3Ply (GameState initialBoard Black)
    
    -- *************************
    -- * SET UP RANDOM OPPONENTS
    let hundredOppsMinusToPlus  =   evalRand (randomGenomes 60 32 (-1.0::Double) (1.0::Double) ) g
    let hundredOppsPlusToPlus   =   evalRand (randomGenomes 50 32 (0.0::Double) (1.0::Double) ) g
    let hundredOppsZerosOnly    =   evalRand (randomGenomes 60 32 (0.0::Double) (0.0::Double) ) g


    -- *************************
    -- * TESTS EA
    -- putStrLn "************************************************** Initial genomes"
    -- print hundredOppsPlusToPlus
    -- putStrLn "END OF INITIAL GENOMES **************************************************"
    -- let res = executeEA 40 pop64 selectionTournament myEval (uniformCrossover 0.75) (mutation 0.07) hundredOppsPlusToPlus g
    -- print res

      -- *************************
    -- * TESTS EA with total replacement
    putStrLn "************************************************** Initial genomes"
    --mapM_ print pop64
    putStrLn "END OF INITIAL GENOMES **************************************************"
    -- let res = executeEAreplacement 2 pop64 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.07) hundredOppsPlusToPlus g
    -- let res = executeEAreplacement 5 2 pop64 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.07) hundredOppsPlusToPlus g
    
    -- let pop = myEval hundredOppsPlusToPlus pop64
    -- putStrLn "************************************************** Initial pop evaluated"
    -- mapM_ print (evalRand pop g)
    -- stats pop g
    -- putStrLn "************************************************** Initial pop evaluated END"

    -- let res = runEA 5 2 pop selectionTournament myEval (uniformCrossover 0.80) (mutation 0.07) hundredOppsPlusToPlus g
    -- print res

    -- 2347 13/03 after long waiting and crash on index too large
    -- let g1 = fun 10 2 pop selectionTournament myEval (uniformCrossover 0.80) (mutation 0.07) hundredOppsPlusToPlus
    -- mapM_ print (evalRand g1 g)
    -- putStrLn "************************************************** GEN1 END"
    -- let g2 = fun 10 2 g1 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.07) hundredOppsPlusToPlus
    -- mapM_ print (evalRand g2 g)
    -- putStrLn "************************************************** GEN2 END"
    -- let g3 = fun 10 2 g2 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.07) hundredOppsPlusToPlus
    -- mapM_ print (evalRand g3 g)
    -- putStrLn "************************************************** GEN3 END"
    -- let g4 = fun 10 2 g3 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.07) hundredOppsPlusToPlus
    -- mapM_ print (evalRand g4 g)
    -- putStrLn "************************************************** GEN4 END"

    -- let g1 = runnnEA 10 2 pop selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus g
    -- mapM_ print (evalRand g1 g)
    -- putStrLn "************************************************** GEN1 END"
    -- let g2 = runnnEA 10 2 g1 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus g
    -- mapM_ print (evalRand g2 g)
    -- putStrLn "************************************************** GEN2 END"
    -- let g3 = runnnEA 10 2 g2 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus g
    -- mapM_ print (evalRand g3 g)
    -- putStrLn "************************************************** GEN3 END"
    -- let g4 = runnnEA 10 2 g3 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus g
    -- mapM_ print (evalRand g4 g)
    -- putStrLn "************************************************** GEN4 END"
    -- -- let tba = funn 2 pop selectionTournament myEval (uniformCrossover 0.80) (mutation 0.07) hundredOppsPlusToPlus

   
   
    -- 16 March
    -- * obtain two parents Tests:
    -- let test2parents = obtain2parents 4 pop selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus
    -- evalprint test2parents g
    -- let test2parents' = obtain2parents 2 test2parents selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus
    -- evalprint test2parents' g
    -- * generationNew Tests:

    -- let gen1 = generationNew 4 pop selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus
    -- stats gen1 g
    -- let gen2 = generationNew 4 gen1 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus
    -- stats gen2 g

    --evalprint gen1 g

    -- * alternative tests
    -- let gen1 = testt 20 2 pop selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus
    -- stats gen1 g
    -- let gen2 = testt 20 2 gen1 selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus
    -- stats gen2 g

    -- evalprint (run 2 2 (myEval hundredOppsPlusToPlus pop64) selectionTournament myEval (uniformCrossover 0.80) (mutation 0.10) hundredOppsPlusToPlus) g

    -- * get a file name to write output to
    filename <- getFileName

    -- * run EA writing to file
    let gs = take 1000 $ executeEA 2 pop64 selection myEval (uniformCrossover 0.60) (mutation 0.10) hundredOppsPlusToPlus g
    let fs = mean gs
    let sts = sdAndvar gs
    let minmax = minAndmaxElems gs
    --mapM_ print $ zip5 gs [1..] fs sts minmax
    writeFile filename $ unlines (map show $ zip5 gs [1..] fs sts minmax)
