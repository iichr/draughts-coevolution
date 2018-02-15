module Evolution where

import Control.Monad.Random
import System.Random.Mersenne.Pure64


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

type Genome a = [a]

-- type FitnessFun a = Genome a -> Int

type SelectionFun a = [(Genome a, Double)] -> Rand PureMT [Genome a]

type Crossover a = (Genome a, Genome a) -> Rand PureMT (Genome a, Genome a)

type Mutation a = Genome a -> Rand PureMT (Genome a)

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



