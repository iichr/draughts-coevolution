{-# Language BangPatterns #-}

module Evolution where

import Data.List (intersperse, concat, foldl', sortBy)
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Control.Monad

import Control.Monad.Random
import System.Random.Mersenne.Pure64

import DraughtsBoard
import Utils
import GamePlay
import MoveSelectTree

import Data.Functor.Identity
import qualified Data.Map.Strict as M


-- ***************************************************
-- *** NON-IO PLAYERS, use with PLAYNONIO function ***
-- ********** SUPPLY PLY AS ARGUMENT *****************
-- ***************************************************

maxplayer :: Int -> Genome Double -> GameState -> Rand PureMT GameState
maxplayer plyLimit genome gs = alphabetadepthlim' negInf posInf 0 plyLimit gs genome getSimpleSum

minplayer :: Int -> Genome Double -> GameState -> Rand PureMT GameState
minplayer plyLimit genome gs = alphabetadepthlimneg' negInf posInf 0 plyLimit gs genome getSimpleSum


-- ******************************************
-- * OTHER PLAYERS *****
-- ******************************************

maxplayerIO :: Int -> Genome Double -> GameState -> IO GameState
maxplayerIO plyLimit genome gs = return $ alphabetadepthlim plyLimit genome getSimpleSum gs

maxplayerNonRand :: Int -> Genome Double -> GameState -> GameState
maxplayerNonRand plyLimit genome gs = alphabetadepthlim plyLimit genome getSimpleSum gs


-- ********************************
-- ***** BOARD SUM FUNCTIONS ******
-- ********************************

-- * WITH DIFFERENT WEIGHTS
-- The sum of the board, i.e. for all squares the value of the piece times the corresponding weight of its position
-- from the weights vector; a positive result: Player White has an advantage, Black does not, 
-- and vice versa for a negative result
getSum :: Genome Double -> GameState -> Double
getSum genome gs = (foldr (+) 0.0 $ getVectortoSum gs genome)
                  + 1.3 * (fromIntegral jumpscount) + 0.7 * (fromIntegral simplemovescount)
--                   + (foldr (+) 0.0 $ getVectortoSumCentrePriority gs genome)

        where 
            jumpscount = length $ getJumps gs
            simplemovescount = length $ getSimpleMoves gs
            
            
            getVectortoSum :: GameState -> Genome Double -> [Double]
            getVectortoSum gs@(GameState (VectorBoard b) _) genome = do
                row <- [0,1,2,5,6,7]
                col <- [0,1,6,7]
                let pos = (row, col)
                let square = getSquare (VectorBoard b) pos
                let i = convertPos2Index pos
                --let weightsvector = makeWeightinit
                let weight = genome !! i
                let w = (pieceVal square) * 1.9 * weight
                return w

            -- rows 3 to 4 incl
            -- cols 2 to 5 incl
            getVectortoSumCentrePriority :: GameState -> Genome Double -> [Double]
            getVectortoSumCentrePriority gs@(GameState (VectorBoard b) _) genome = do
                row <- [3..4]
                col <- [2..5]
                let pos = (row, col)
                let square = getSquare (VectorBoard b) pos
                let i = convertPos2Index pos
                --let weightsvector = makeWeightinit
                let weight = genome !! i
                let w = (pieceVal square) * 3 * weight
                return w


-- * JUST THE SUM OF THE BOARD

getSimpleSum :: Genome Double -> GameState -> Double
getSimpleSum genome gs= foldl' (+) 0.0 $ getVectortoSum gs genome
         where    
             getVectortoSum :: GameState -> Genome Double -> [Double]
             getVectortoSum gs@(GameState (VectorBoard b) _) genome = do
                 pos <- allPositionsInOrder
                 let square = getSquare (VectorBoard b) pos
                 let i = M.lookup pos positionIndicesMap
                 if isNothing i
                    then
                        return $ 0
                    else
                        return $ pieceVal square * (genome !! (fromJust i))
                 -- let i = convertPos2Index pos
                 -- let weight = genome !! i
                 --let w = pieceVal square * weight
                 --return w


-- ******************************************************
-- ********** EVALUATION (FITNESS) FUNCTION *************
-- ******************************************************

coin :: RandomGen g => Rand g Int
coin = getRandomR (0,1)


-- EVALUATION (fitness)
-- given a population of genomes P
-- generate 100 fixed oponents, keep them
-- play EACH individual from p
-- count how many times each individual won
-- zip that with the genome in the form of (genome, number of wins)
-- at the end map to obtain a list of [(genome, number of wins)]
evaluate :: Genome Double -> Genome Double -> Rand PureMT Int
evaluate gen1 gen2 = do
    toss <- coin
    -- one is maximising i.e Black first position
    let maximiser = maxplayer 4 
    -- two is minimising i.e second position always
    let minimiser = minplayer 4

    let gs = (GameState initialBoard Black)
    let singleGenomeScores = if (toss == 0)
        then
            -- black is gen1 + maximising moves
            -- white is gen2 + minimising moves
            playnonIO' 150 gen1 gen2 maximiser minimiser gs
        else
            -- black is gen2 + maximising
            -- white is gen1 + minimising
            playnonIO' 150 gen2 gen1 maximiser minimiser gs 
    --let ones = length (filter (==(1)) singleGenomeScores)
    --let minusones = length (filter (==(-1)) singleGenomeScores)
    --let draws = length (filter (==(0)) singleGenomeScores)
    --return $ (genome, singleGenomeScores)
    singleGenomeScores         

    -- where
    --     singleGenomeScores = [score | o <- opponents, let score = playnonIO 150 o genome ai ai gs]
    --     ai = performMoveAIalphabeta4PlyNonIO
    --     gs = (GameState initialBoard Black)
        
        -- ones = length (filter (==(1)) singleGenomeScores)
        -- minusones = length (filter (==(-1)) singleGenomeScores)
        -- draws = length (filter (==(0)) singleGenomeScores)

-- Get the outcome of playing a game for up to 150 moves given two genomes: 
-- gen1 the maximiser (Black),
-- gen2 the minimiser (White)
evaluateNoCoin :: Genome Double -> Genome Double -> Rand PureMT Int
evaluateNoCoin gen1 gen2 = playnonIO' 150 gen1 gen2 maximiser minimiser gs
        where
            maximiser = maxplayer 1
            minimiser = minplayer 1
            gs = GameState initialBoard Black


-- Play a White genome against 15 randomly selected Black opponents
evaluateNoCoinAgainstMultiple :: [Genome Double] -> Genome Double -> Rand PureMT Int
evaluateNoCoinAgainstMultiple blackopps whitegen = do
    opps20randomBlack <- randomNopponents 25 blackopps 
    allresults <- mapM (\blackop -> evaluateNoCoin blackop whitegen) opps20randomBlack
    -- filter just white wins as whitegen is White
    return $ length $ filter (==(-1)) allresults


-- Play a Black genome against 15 randomly selected White opponents
evaluateNoCoinAgainstMultipleBlack :: [Genome Double] -> Genome Double -> Rand PureMT Int
evaluateNoCoinAgainstMultipleBlack whiteopps blackgen = do
    opps20randomWhite <- randomNopponents 25 whiteopps
    allresults <- mapM (\whiteop -> evaluateNoCoin blackgen whiteop) opps20randomWhite
    -- filter just black wins
    return $ length $ filter (==(1)) allresults


-- Given a list of genomes and a list of genome opponents evaluate each genome against all of the opponents
-- and return the number of wins (-1.0s) against them (provided we play as White)
evaluateToTuple  :: [Genome Double] -> [Genome Double] -> [(Genome Double,  Rand PureMT Int)]
evaluateToTuple opps gen1s = do
    gen1 <- gen1s
    let evalscore = evaluateNoCoinAgainstMultiple opps gen1
    map ((,) gen1) [evalscore]


-- Given a player and two lists of genomes ordered Black White
-- evaluate each genome against 15 randomly selected opponents of the specified player and return the number of wins (-1.0s / 1.0s) against them
evaluateToTupleCoEv  :: Player -> [Genome Double] -> [Genome Double] -> [(Genome Double,  Rand PureMT Int)]
evaluateToTupleCoEv player1 blackgenomes whitegenomes = do
    if player1 == Black
        then do
            -- each black against all of the opposing white genomes
            b <- blackgenomes
            -- we are evaluating against White opponents
            let evalscore = evaluateNoCoinAgainstMultipleBlack whitegenomes b
            map ((,) b) [evalscore]
    else do
        -- each white against all of the opposing black genomes
        w <- whitegenomes
        let evalscore = evaluateNoCoinAgainstMultiple blackgenomes w
        map ((,) w) [evalscore]


-- Unpacking a tuple within a monad to a monadic tuple   
toMonadTuple :: [(Genome Double, Rand PureMT Int)] -> Rand PureMT [(Genome Double, Int)]
toMonadTuple [] = return $ []
toMonadTuple ((a,b):xs) = do
   y <- b
   z <- toMonadTuple xs
   --return $ foldr (:) [(a,y+2)] z
   return $ (a,y):z

test = do
    -- evalRand returns value ONLY
    -- runRand return value AND generator in a tuple (v,g)
    g <- newPureMT 
    let p = evalRand (randomGenomes 10 1 (0.1::Double) (1.0::Double) ) g
    -- crossover function test
    let z = evalRand (uniformCrossover 0.75 (head p,[4,5,6])) g
    -- mutations test
    let k = evalRand (mutation 0.75 [1,2,3,4]) g
    -- do batch crossovers test
    let y = evalRand (doCrossovers p (uniformCrossover 0.75)) g
    -- selection
    let samplePopFitnessTuples = zip p [0,4,3,7,8,9,1,5,6,3]
    let sel = evalRand (selectionTournament samplePopFitnessTuples 2) g
    print "Population"
    print p
    print "Population,Fitness tuples"
    -- mapM for neat and tidy printing one by one
    mapM_ print samplePopFitnessTuples
    --print y
    print "Tournament selection"
    mapM_ print sel

-- ******************************************************
-- ************* EVOLUTIONARY WRAPPERS ******************
-- ******************************************************

-- Modify value with probability p. Unaltered is hence 1-p
withProbability p funmodify x = do
  t <- getRandomR (0.0, 1.0)
  if t < p
    then funmodify x
    else return x

-- Tournament selection
-- pick two individuals uniformly at random
-- choose the one with highest fitness
-- do TWO times for two parents
-- ! NEVER CALL WITH 0
-- * population and number of times to compete as inputs
selectionTournament :: [(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)
selectionTournament pop k = selectionLoop pop ([],0) k 
    where
        selectionLoop :: [(Genome Double, Int)] -> (Genome Double, Int) -> Int -> Rand PureMT (Genome Double)
        selectionLoop pop chosen k
            | k > 0 = do
                i <- getRandomR (0, (length pop)-1)
                let newlySelected = pop !! i
                -- if newlySelected is 0 and chosen is 0 or less, pick the newlySelected genome
                if snd newlySelected == 0 && snd newlySelected >= snd chosen
                    then selectionLoop pop newlySelected (k-1)
                -- if newlySelected is less than previously chosen, preserve chosen
                else if snd newlySelected <= snd chosen
                    then selectionLoop pop chosen (k-1)
                -- else newlySelected is better so set it as chosen for the next tournament
                else
                    selectionLoop pop newlySelected (k-1)
            | otherwise = return $ fst chosen


-- ******************************************************
-- ************* EVOLUTIONARY OPERATORS *****************
-- ******************************************************

-- * CROSSOVER
uniformCrossover :: Double -> Crossover Double
uniformCrossover p (g1,g2) = do
    (h1, h2) <- unzip `liftM` mapM swap (zip g1 g2)
    return (h1,h2)
    where
        swap = withProbability p (\(a,b) -> return (b,a))


doCrossovers :: [Genome Double] -> Crossover Double -> Rand PureMT [Genome Double]
doCrossovers [] _ = return []
-- return nothing if only a single genome
doCrossovers [_] _ = return []
doCrossovers (g1:g2:gs) crossoverFun = do
    (g1new, g2new) <- crossoverFun (g1, g2)
    gsnew <- doCrossovers gs crossoverFun
    return $ g1new:g2new:gsnew


-- * MUTATION
mutation :: Double -> Mutation Double
mutation p genome = do
    t <- getRandomR (0.0, 1.0)
    if t < p
        then do
            nrElems <- getRandomR (0, length genome -1)
            k <- getRandomR(-1.0, 0.0)
            return ((take nrElems genome) ++ k:(drop (nrElems+1) genome))
        else
            return genome


-- * SELECTION
-- Given a genome,fitness tuple and number of times to compete in the tournament
-- specify generational replacement:
-- if called with (take n $ elitism pop) we will preserve the top n from the population
-- if called with [] = full generational replacement
selection :: [(Genome Double, Int)] -> Int -> Rand PureMT [Genome Double]
selection pop k = select' []
    where
        select' gs' =
            -- stop condition, how many times to do selection
            if length gs' >= length pop 
                then return gs'
            else do
                g1 <- selectionTournament pop k
                g2 <- selectionTournament pop k
                let newPop = g1:g2:gs'
                select' newPop


-- * EVALUATION function combining evaluateToTuple and toMonadTuple
myEval :: [Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]
myEval opps gen1s = toMonadTuple $ evaluateToTuple opps gen1s


-- * EVALUATION function for CO-EVOLUTION combining evaluateToTuple and toMonadTuple
-- If player1 == Black: evaluate each black against all white using evaluateNoCoinAgainstMultipleBlack opps b, 1
-- else : evaluate each white against all black using evaluateNoCoinAgainstMultiple opps w, -1
myEvalCoEv :: Player -> [Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]
myEvalCoEv player1 blackgenomes whitegenomes = toMonadTuple $ evaluateToTupleCoEv player1 blackgenomes whitegenomes

-- **********************************************
-- **** EVOLUTION GENERATION + EXECUTION ********
-- **********************************************


-- * Generate infinitely many populations recursively, appending them to the previous ones
-- * using the provided 
-- tournament size for tournament selection,
-- function to evaluate a population (assign it fitness values)
-- selection function (tournament selection currently)
-- crossover function
-- mutation function
-- and a fixed number of opponents to evaluate against to obtain fitness values
generations :: Int ->
    [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT [Genome Double]) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    Rand PureMT [[(Genome Double, Int)]]
generations tournSize !pop selectionFun evalFun crossoverFun mutationFun opps = do
    newGen <- selectionFun pop tournSize
    newGen  <- doCrossovers newGen crossoverFun
    newGen <- mapM mutationFun newGen
    zippednewPop <- evalFun opps newGen
    nextGens <- generations tournSize zippednewPop selectionFun evalFun crossoverFun mutationFun opps
    return $ pop : nextGens


-- * Execute an evolutionary algorithm, runs infinitely many times producing a list of populations.
-- * Takes an already evaluated initial population and calculates the fitness
-- * of all its individuals. In addition provide
-- a tournament size for tournament selection,
-- a selection function (tournament selection currently)
-- a function to evaluate a population (assign it fitness values)
-- a crossover function
-- a mutation function
-- a fixed number of opponents to evaluate against to obtain fitness values
-- a PureMT generator for evaluting the final population
executeEA :: Int ->
    [Genome Double] ->
    ([(Genome Double, Int)] -> Int -> Rand PureMT [Genome Double]) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) -> 
    Crossover Double ->
    Mutation Double ->
    [Genome Double] -> 
    PureMT ->
    [[(Genome Double, Int)]]
executeEA tournSize startPop selectionFun evalFun crossoverFun mutationFun opps g =
    let p = evalRand (evalFun opps startPop) g
    in evalRand (generations tournSize p selectionFun evalFun crossoverFun mutationFun opps) g

-- *****************
-- * Co-Evolution **
-- *****************

-- * pop is White, opps are Black
-- * Generate infinitely many populations recursively, appending them to the previous ones
-- * using the provided
-- tournament size for tournament selection,
-- selection function (tournament selection currently)
-- function to evaluate a population (assign it fitness values) - player to evaluate, black, white in order
-- crossover function
-- mutation function
-- and a fixed number of opponents to evaluate against to obtain fitness values
generationsCOEA :: Int ->
    [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT [Genome Double]) ->
    (Player -> [Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [(Genome Double, Int)] -> 
    Rand PureMT [[(Genome Double, Int)]]
generationsCOEA tournSize !pop selectionFun evalFunCOEA crossoverFun mutationFun !opps = do
    newPopWhite <- selectionFun pop tournSize
    newOppsBlack <- selectionFun opps tournSize

    newPopWhite  <- doCrossovers newPopWhite crossoverFun
    newOppsBlack <- doCrossovers newOppsBlack crossoverFun

    newPopWhite <- mapM mutationFun newPopWhite
    newOppsBlack <- mapM mutationFun newOppsBlack

    zippednewPopWhite <- evalFunCOEA White newOppsBlack newPopWhite
    zippednewOppsBlack <- evalFunCOEA Black newOppsBlack newPopWhite

    nextGens <- generationsCOEA tournSize zippednewPopWhite selectionFun evalFunCOEA crossoverFun mutationFun zippednewOppsBlack
    return $ pop : opps : nextGens


executeCOEA :: Int ->
    [Genome Double] ->
    ([(Genome Double, Int)] -> Int -> Rand PureMT [Genome Double]) ->
    (Player -> [Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) -> 
    Crossover Double ->
    Mutation Double ->
    [Genome Double] -> 
    PureMT ->
    [[(Genome Double, Int)]]
executeCOEA tournSize startPop selectionFun evalFunCOEA crossoverFun mutationFun startOpps g =
    let pop = evalRand (evalFunCOEA White startOpps startPop) g
        opps = evalRand (evalFunCOEA Black startOpps startPop) g
    in evalRand (generationsCOEA tournSize pop selectionFun evalFunCOEA crossoverFun mutationFun opps) g