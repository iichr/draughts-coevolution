module Main where

import System.IO
import Control.Monad.Random
import System.Random.Mersenne.Pure64
import Data.List
import Data.Ord

import Lib
import Utils
import DraughtsBoard
import GamePlay
import MoveSelectTree
import MoveSelectMechanical
import Evolution

-- * Final evaluation of two populations
-- * Used to verify the hypothesis
finalEval :: PureMT -> [Genome Double] -> [Genome Double] -> [(Int, Int)]
finalEval gen whitepop blackpop = eval gen whitepop blackpop
    where
        eval :: PureMT -> [Genome Double] -> [Genome Double] -> [(Int, Int)]
        eval gen whitepop blackpop = do 
            w <- whitepop
            -- evaluate against opponents(Black)
            let evalscore = evalRand (evaluateFinalReport blackpop w) gen
            let wins = length $ filter (==(-1)) evalscore
            let draws = length $ filter (==(0)) evalscore
            return (wins,draws)


main :: IO ()
main = do
    g <- newPureMT
    -- The output is written immediately (never stored in the buffer)
    hSetBuffering stdout NoBuffering

    startDateTimeStamp <- ("Began at: " ++) <$> getDateTime
    print startDateTimeStamp

    let randomPopulationFile = "bestrandom07.txt"
    print $ "Testing deterministic against " ++ randomPopulationFile
    
    -- ***************************
    -- * FINAL EVALUATION
    -- ***************************

    -- * TO BE DONE ONCE INITIALLY, COMMENT OUT OTHERWISE
    -- * Extract 2000 random individuals from the deterministically evolved population
    -- * so that we can onbjectively compare the influence of randomness against the same
    -- * individuals every time.
    -- * Random sample necessary due to the time and computational resoruce infeasibility of running the full evaluation
    -- exportRandomPopulationSample 2000 "deterministic15against400gen.txt" "2000randomlychosendeterministic.txt" g 

    -- Get the best populations from their respective files, deterministic always Black
    whiterandomPop <- getApopulationFromFile randomPopulationFile
    blackdeterministicPop <- getApopulationFromFile "2000randomlychosendeterministic.txt"

    let tupleEvaluatedZipped = finalEval g whiterandomPop blackdeterministicPop
    print tupleEvaluatedZipped
    let tupleEvaluated = unzip tupleEvaluatedZipped
    -- Sum up the number of wins and the number of draws and print the result 
    let res = (foldl' (+) 0 $ fst $ tupleEvaluated, foldl' (+) 0 $ snd $ tupleEvaluated)
    print res

    -- ***************************
    -- *** MAIN COEVOLUTION ******
    -- ***************************

    -- -- * Get two file names to write the statistics (ready for plotting) and the output
    -- filename <- fst getFileName
    -- statsFileName <- snd getFileName
    -- print $ "Populations written to " ++ filename
    -- print $ "Stats written to " ++ statsFileName

    -- -- * PARAMETER SETUP
    -- let populationSize = 150 :: Int
    -- let oppsSize = 150 :: Int
    -- let numGenerations = 400 :: Int

    -- let crossoverRate = 0.75 :: Double
    -- let mutationRate = 0.2 :: Double
    -- let tournamentSize = 5 :: Int

    -- print $ "Pop size: " ++ show populationSize
    -- print $ "Opps size: " ++  show oppsSize
    -- print $ "Generations: " ++  show numGenerations
    -- print $ "Crossover rate: " ++  show crossoverRate
    -- print $ "Mutation rate: " ++  show mutationRate
    -- print $ "Tournament size: " ++  show tournamentSize
    -- print "Evaluated against how many randomly selected: 25 "
    -- print "Randomness limneg': 0.2"
    -- print "Randomness lim': 0.2"
    
    -- -- ** PROVIDING AN EXISTING POPULATION IF NECESSARY (IF NOT USING RANDOMLY GENERATED)
    -- -- initialPopulation <- getApopulationFromFile "bestrandom02.txt"
    -- -- opponents <- getApopulationFromFile "150oppsFIRST.txt"

    -- -- ** RANDOMLY GENERATED POPULATIONS OF 32 CHROMOSOMES EACH, RANGING FROM -1.0 to 1.0
    -- let initialPopulation = evalRand (randomGenomes populationSize 32 (-1.0::Double) (1.0::Double) ) g
    -- let opponents         = evalRand (randomGenomes oppsSize 32 (-1.0::Double) (1.0::Double) ) g


    -- -- * COEVOLUTION
    -- let result = take numGenerations $ executeCOEA tournamentSize initialPopulation selection myEvalCoEv (uniformCrossover crossoverRate) (mutation mutationRate) opponents g
    -- let fs = mean result
    -- let sts = sdAndvar result
    -- let minmax = minAndmaxElems result
    -- let evStats = getEvolutionaryStats fs sts minmax
    -- print evStats
    -- -- * Write stats to stat file
    -- writeFile statsFileName evStats
    -- -- * Write population and stats to separate file
    -- writeFile filename $ unlines (map show $ zip5 result [1..] fs sts minmax)
    
    finalDateTimeStamp <- ("Finished at: " ++) <$> getDateTime
    print finalDateTimeStamp
