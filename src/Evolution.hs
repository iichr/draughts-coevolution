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
    let p = evalRand (randomGenomes 3 5 (0.1::Double) (1.0::Double) ) g
    -- crossover function test
    let z = evalRand (uniformCrossover 0.75 (head p,[4,5,6])) g
    -- mutations test
    let k = evalRand (mutate 0.75 [1,2,3,4]) g
    -- do batch crossovers test
    let y = evalRand (doCrossovers p (uniformCrossover 0.75)) g
    --return z
    print p
    return y


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

-- TODO selection - tournament - pick 2, compare them

-- TODO evaluation
-- TODO gather statistics
-- TODO consider replacing lists with Unboxed Vector - O(1) access worth it? 

-- Generate 100 random strategies y1-y100
-- hundredGenomes = randomGenomes 100 64 (0.1::Double) (1.0::Double)

-- fitness
-- play one individual against a fixed 100 opponents from above
-- record total number of wins
-- needs to produce a Double

doCrossovers :: [Genome Double] -> Crossover Double -> Rand PureMT [Genome Double]
doCrossovers [] _ = return []
-- return nothing if only a single genome
doCrossovers [_] _ = return []
doCrossovers (g1:g2:gs) crossoverFun = do
    (g1new, g2new) <- crossoverFun (g1, g2)
    gsnew <- doCrossovers gs crossoverFun
    return $ g1new:g2new:gsnew
