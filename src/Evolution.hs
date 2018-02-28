module Evolution where

import Data.List (intersperse, concat, foldl')
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Control.Monad

import Control.Monad.Random
import System.Random.Mersenne.Pure64

import DraughtsBoard
import Utils
import GamePlay
import MoveSelectTree


-- ***********
-- * EXAMPLE *
-- ***********
die :: RandomGen g => Rand g Int
die = getRandomR (1,6)

dice :: RandomGen g => Int -> Rand g [Int]
dice n = sequence (replicate n die)

testt = do
    rolls <- evalRandIO $ dice 10
    print rolls

-- ***********
-- * END OF EXAMPLE *
-- ***********



-- ***************************************************
-- *** NON-IO PLAYERS, use with PLAYNONIO function ***
-- ********** SUPPLY PLY AS ARGUMENT *****************
-- ***************************************************

maxplayer :: Int -> Genome Double -> GameState -> Rand PureMT GameState
maxplayer plyLimit genome gs = alphabetadepthlim' negInf posInf 0 plyLimit gs genome getSum

minplayer :: Int -> Genome Double -> GameState -> Rand PureMT GameState
minplayer plyLimit genome gs = alphabetadepthlimneg' negInf posInf 0 plyLimit gs genome getSum


-- ******************************************
-- * OTHER PLAYERS *****
-- ******************************************

maxplayerIO :: Int -> Genome Double -> GameState -> IO GameState
maxplayerIO plyLimit genome gs = return $ alphabetadepthlim plyLimit genome getSum gs

maxplayerNonRand :: Int -> Genome Double -> GameState -> GameState
maxplayerNonRand plyLimit genome gs = alphabetadepthlim plyLimit genome getSum gs


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
                let square = fromJust $ getSquare (VectorBoard b) pos
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
                let square = fromJust $ getSquare (VectorBoard b) pos
                let i = convertPos2Index pos
                --let weightsvector = makeWeightinit
                let weight = genome !! i
                let w = (pieceVal square) * 3 * weight
                return w


-- * JUST THE SUM OF THE BOARD

getSimpleSum :: Genome Double -> GameState -> Double
getSimpleSum genome gs= foldr (+) 0.0 $ getVectortoSum gs genome
         where    
             getVectortoSum :: GameState -> Genome Double -> [Double]
             getVectortoSum gs@(GameState (VectorBoard b) _) genome = do
                 row <- [0..7]
                 col <- [0..7]
                 let pos = (row, col)
                 let square = fromJust $ getSquare (VectorBoard b) pos
                 let i = convertPos2Index pos
                 let weight = genome !! i
                 let w = pieceVal square * weight
                 return w



randomGenomes :: (RandomGen g, Random a, Enum a) => Int -> Int -> a -> a -> Rand g [Genome a]
randomGenomes len genomeLen from to = do
    l <- replicateM (len*genomeLen) $ getRandomR (from,to)
    return $ nLists genomeLen l
    where nLists :: Int -> [a] -> [[a]]
          nLists _ [] = []
          nLists n ls = take n ls : nLists n (drop n ls)

test = do
    -- evalRand returns value ONLY
    -- runRand return value AND generator in a tuple (v,g)
    g <- newPureMT 
    let p = evalRand (randomGenomes 10 1 (0.1::Double) (1.0::Double) ) g
    -- crossover function test
    let z = evalRand (uniformCrossover 0.75 (head p,[4,5,6])) g
    -- mutations test
    let k = evalRand (mutate 0.75 [1,2,3,4]) g
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


uniformCrossover :: Double -> Crossover Double
uniformCrossover p (g1,g2) = do
    (h1, h2) <- unzip `liftM` mapM swap (zip g1 g2)
    return (h1,h2)
    where
        swap = withProbability p (\(a,b) -> return (b,a))

-- Modify value with probability p. Unaltered is hence 1-p
withProbability p funmodify x = do
  t <- getRandomR (0.0, 1.0)
  if t < p
    then funmodify x
    else return x

mutate :: Double -> Mutation Double
mutate p genome = do
    t <- getRandomR (0.0, 1.0)
    if t < p
        then do
            nrElems <- getRandomR (0, length genome -1)
            return ((take nrElems genome) ++ t:(drop (nrElems+1) genome))
        else
            return genome

-- TODO gather statistics
-- TODO consider replacing lists with Unboxed Vector - O(1) access worth it? 

-- Generate 100 random strategies y1-y100
-- hundredGenomes = randomGenomes 100 64 (0.1::Double) (1.0::Double)

doCrossovers :: [Genome Double] -> Crossover Double -> Rand PureMT [Genome Double]
doCrossovers [] _ = return []
-- return nothing if only a single genome
doCrossovers [_] _ = return []
doCrossovers (g1:g2:gs) crossoverFun = do
    (g1new, g2new) <- crossoverFun (g1, g2)
    gsnew <- doCrossovers gs crossoverFun
    return $ g1new:g2new:gsnew


-- tournament selection
-- pick two individuals uniformly at random
-- choose the one with highest fitness
-- do TWO times for two parents
-- * population and number of times to compete as inputs
selectionTournament :: [(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)
selectionTournament pop k = selectionLoop pop ([],0) k 
    where
        selectionLoop :: [(Genome Double, Int)] -> (Genome Double, Int) -> Int -> Rand PureMT (Genome Double)
        selectionLoop pop chosen k
            | k > 0 = do
                i <- getRandomR (0, (length pop)-1)
                let ind = pop !! i
                -- ! DUPLICATES DISCARDED - effect on evolution?
                if snd ind <= snd chosen
                    -- keep previously chosen genome and carry on
                    then selectionLoop pop chosen (k-1)
                    -- else replace current best with with the new individual
                    else selectionLoop pop ind (k-1)
            | otherwise = return $ fst chosen


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


evaluateNoCoin :: Genome Double -> Genome Double -> Rand PureMT Int
evaluateNoCoin gen1 gen2 = playnonIO' 150 gen1 gen2 maximiser minimiser gs
        where
            maximiser = maxplayer 4
            minimiser = minplayer 4
            gs = GameState initialBoard Black


