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
            maximiser = maxplayer 1
            minimiser = minplayer 1
            gs = GameState initialBoard Black


evaluateNoCoinAgainstMultiple :: [Genome Double] -> Genome Double -> Rand PureMT Int
evaluateNoCoinAgainstMultiple opps gen1 = do 
    allresults <- mapM (\op -> evaluateNoCoin op gen1) opps
    -- filter just white wins as gen1 is White
    return $ length $ filter (==(-1)) allresults


-- Given a list of genomes and a list of genome opponents evaluate each genome against all of the opponents
-- and return the number of wins (-1.0s) against them (provided we play as White)
evaluateToTuple  :: [Genome Double] -> [Genome Double] -> [(Genome Double,  Rand PureMT Int)]
evaluateToTuple opps gen1s = do
    gen1 <- gen1s
    let evalscore = evaluateNoCoinAgainstMultiple opps gen1
    map ((,) gen1) [evalscore]


-- Unpacking a tuple within a monad to a monadic tuple   
toMonadTuple :: [(Genome Double, Rand PureMT Int)] -> Rand PureMT [(Genome Double, Int)]
toMonadTuple [] = return $ []
toMonadTuple ((a,b):xs) = do
   y <- b
   z <- toMonadTuple xs
   --return $ foldr (:) [(a,y+2)] z
   return $ [(a,y)] ++ z


-- evaluation function combining evaluateToTuple and toMonadTuple
myEval :: [Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]
myEval opps gen1s = toMonadTuple $ evaluateToTuple opps gen1s

-- * Generate population using the following provided:
-- * list of (genome, fitness) tuples
-- * selection function taking such a tuple - do 2 times to obtain 2 parents
-- / current implementation is based on tournament selection
-- / a value for number of competitions needs to be specified
-- *! evaluation function - needs to produce same outcome as popfitnesstuple!
-- / evaluateToTuple Ours Opponents :: [(Genome Double,  Rand PureMT Int)]
-- / then call toMonadTuple on that
-- * crossover function - apply to the 2 parents resulting from mutation
-- * mutation function - apply a mutation to the 2 parents from selection
-- * list of opponent genomes to play against
generation :: Rand PureMT [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    Rand PureMT [(Genome Double, Int)]
generation pop selectionFun evalFun crossoverFun mutationFun opps = do
    popFitnessTuple <- pop
    -- SELECTION parent 1 - requires genomes to be zipped with their fitness
    parent1 <- selectionFun popFitnessTuple 4
    -- SELECTION parent 2 - requires genomes to be zipped with their fitness
    parent2 <- selectionFun popFitnessTuple 4
    let parents = parent1:parent2:[] -- [Genome a]
    -- MUTATION
    parents <- mapM mutationFun parents
    -- CROSSOVER
    parents <- doCrossovers parents crossoverFun
    -- EVALUATION (FITNESS, obtain a list of (genome,fitness) pairs
    let newPop = evalFun opps parents
    -- let newGen = [evalFun p opp | p <- parents, opp <- opps]
    -- (mapM (\opps -> evaluateNoCoin opps genOnesOnly) hundredOppsPlusToPlus)
    nextGenerationPop <- generation newPop selectionFun evalFun crossoverFun mutationFun opps
    return $ (popFitnessTuple ++ nextGenerationPop)


-- Execute the evolutionary algorithm by providing a random number generator
-- runs for a number of generations k
executeEA :: Int -> 
    [Genome Double] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    PureMT ->
    [(Genome Double, Int)]
executeEA k initialPop selectionFun evalFun crossoverFun mutationFun opps g =
    take k $ evalRand (generation pop selectionFun evalFun crossoverFun mutationFun opps) g
    where
        -- evaluate intitial population to a list of (genome, fitness) tuples
        pop = evalFun opps initialPop


-- * Generate population using the following provided:
-- * list of (genome, fitness) tuples
-- * selection function taking such a tuple - do 2 times to obtain 2 parents
-- / current implementation is based on tournament selection
-- / a value for number of competitions needs to be specified
-- *! evaluation function - needs to produce same outcome as popfitnesstuple!
-- / evaluateToTuple Ours Opponents :: [(Genome Double,  Rand PureMT Int)]
-- / then call toMonadTuple on that
-- * crossover function - apply to the 2 parents resulting from mutation
-- * mutation function - apply a mutation to the 2 parents from selection
-- * list of opponent genomes to play against
generationTotalReplacement :: Int ->
    Rand PureMT [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    Rand PureMT [(Genome Double, Int)]
generationTotalReplacement tournSize pop selectionFun evalFun crossoverFun mutationFun opps = do
    popFitnessTuple <- pop
    let count = length popFitnessTuple
    -- SELECTION parent 1 - requires genomes to be zipped with their fitness
    parent1 <- selectionFun popFitnessTuple tournSize
    -- SELECTION parent 2 - requires genomes to be zipped with their fitness
    parent2 <- selectionFun popFitnessTuple tournSize
    let parents = parent1:parent2:[] -- [Genome a]
    -- MUTATION
    parents <- mapM mutationFun parents
    -- CROSSOVER
    parents <- doCrossovers parents crossoverFun
    -- EVALUATION (FITNESS, obtain a list of (genome,fitness) pairs
    newPop <- evalFun opps parents
    -- let newGen = [evalFun p opp | p <- parents, opp <- opps]
    -- (mapM (\opps -> evaluateNoCoin opps genOnesOnly) hundredOppsPlusToPlus)
    nextGenerationPop <- generationTotalReplacement' (count-2) (tournSize) pop selectionFun evalFun crossoverFun mutationFun opps
    return $ (newPop ++ nextGenerationPop)


generationTotalReplacement' :: (Int) -> (Int) ->
    Rand PureMT [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    Rand PureMT [(Genome Double, Int)]
generationTotalReplacement' count tournSize pop selectionFun evalFun crossoverFun mutationFun opps
   | count > 0 = do
        popFitnessTuple <- pop
        -- SELECTION parent 1 - requires genomes to be zipped with their fitness
        parent1 <- selectionFun popFitnessTuple tournSize
        -- SELECTION parent 2 - requires genomes to be zipped with their fitness
        parent2 <- selectionFun popFitnessTuple tournSize
        let parents = parent1:parent2:[] -- [Genome a]
        -- MUTATION
        parents <- mapM mutationFun parents
        -- CROSSOVER
        parents <- doCrossovers parents crossoverFun
        -- EVALUATION (FITNESS, obtain a list of (genome,fitness) pairs
        newPop <- evalFun opps parents
        nextGenerationPop <- generationTotalReplacement' (count-2) tournSize pop selectionFun evalFun crossoverFun mutationFun opps
        return $ (newPop ++ nextGenerationPop)
    -- make otherwise case execute a new generation from the one just produced when the count is over?
    | otherwise = return $ []

-- Execute the evolutionary algorithm by providing a random number generator
-- runs only for one generation
executeEAonce :: Int -> 
    Rand PureMT [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    Rand PureMT [(Genome Double, Int)]
executeEAonce tournSize initialPop selectionFun evalFun crossoverFun mutationFun opps =
    -- take k $ evalRand (generationTotalReplacement tournSize pop selectionFun evalFun crossoverFun mutationFun opps) g
    generationTotalReplacement tournSize initialPop selectionFun evalFun crossoverFun mutationFun opps
    --where
        -- evaluate intitial population to a list of (genome, fitness) tuples
        --pop = evalFun opps initialPop


executeEAreplacement :: Int -> 
    Int ->
    [Genome Double] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    PureMT ->
    [(Genome Double, Int)]
executeEAreplacement nrGens tournSize initialPop selectionFun evalFun crossoverFun mutationFun opps g = do
    let pop = evalFun opps initialPop
    -- Rand PureMT [(Genome Double, Int)]
    let firstGen = generationTotalReplacement tournSize pop selectionFun evalFun crossoverFun mutationFun opps
    if nrGens > 0
        then executeEAreplacement' (nrGens - 1) tournSize firstGen selectionFun evalFun crossoverFun mutationFun opps g
    else
        evalRand firstGen g

executeEAreplacement' :: Int -> 
    Int ->
    Rand PureMT [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    PureMT ->
    [(Genome Double, Int)]
executeEAreplacement' nrGens tournSize population selectionFun evalFun crossoverFun mutationFun opps g = do
    if nrGens > 0
        then do
            let nextGen = generationTotalReplacement tournSize population selectionFun evalFun crossoverFun mutationFun opps
            executeEAreplacement' (nrGens-1) tournSize nextGen selectionFun evalFun crossoverFun mutationFun opps g 
    else
        evalRand population g


-- third attempt
-- change signature to accept already evaluated popualtion (do beforehand in main)
runEA :: Int -> Int ->
    Rand PureMT [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    PureMT ->
    [(Genome Double, Int)]
runEA nrGens tournSize pop selectionFun evalFun crossoverFun mutationFun opps g = do
    -- take k $ evalRand (generationTotalReplacement tournSize pop selectionFun evalFun crossoverFun mutationFun opps) g
    let nextGen = generationTotalReplacement tournSize pop selectionFun evalFun crossoverFun mutationFun opps
    if nrGens > 0 
        then runEA (nrGens-1) tournSize nextGen selectionFun evalFun crossoverFun mutationFun opps g
    else evalRand (nextGen) g

    -- where
    --     -- evaluate intitial population to a list of (genome, fitness) tuples
    --     pop = evalFun opps initialPop


-- 23.27
-- * Generate population using the following provided:
-- * list of (genome, fitness) tuples
-- * selection function taking such a tuple - do 2 times to obtain 2 parents
-- / current implementation is based on tournament selection
-- / a value for number of competitions needs to be specified
-- *! evaluation function - needs to produce same outcome as popfitnesstuple!
-- / evaluateToTuple Ours Opponents :: [(Genome Double,  Rand PureMT Int)]
-- / then call toMonadTuple on that
-- * crossover function - apply to the 2 parents resulting from mutation
-- * mutation function - apply a mutation to the 2 parents from selection
-- * list of opponent genomes to play against
fun :: Int -> Int ->
    Rand PureMT [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    Rand PureMT [(Genome Double, Int)]
fun 0 tournSize pop selectionFun evalFun crossoverFun mutationFun opps = do
    return $ []
fun count tournSize pop selectionFun evalFun crossoverFun mutationFun opps = do
    popFitnessTuple <- pop
    -- SELECTION parent 1 - requires genomes to be zipped with their fitness
    parent1 <- selectionFun popFitnessTuple tournSize
    -- SELECTION parent 2 - requires genomes to be zipped with their fitness
    parent2 <- selectionFun popFitnessTuple tournSize
    let parents = parent1:parent2:[] -- [Genome a]
    -- MUTATION
    parents <- mapM mutationFun parents
    -- CROSSOVER
    parents <- doCrossovers parents crossoverFun
    -- EVALUATION (FITNESS, obtain a list of (genome,fitness) pairs
    newPop <- evalFun opps parents
    nextGen <- fun (count-2) tournSize pop selectionFun evalFun crossoverFun mutationFun opps
    return $ foldr (:) newPop nextGen
    --return $! newPop ++ nextGen


funn :: Int ->
    Rand PureMT [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    Rand PureMT [(Genome Double, Int)]
funn tournSize pop selectionFun evalFun crossoverFun mutationFun opps = do
    popFitnessTuple <- pop
    -- SELECTION parent 1 - requires genomes to be zipped with their fitness
    parent1 <- selectionFun popFitnessTuple tournSize
    -- SELECTION parent 2 - requires genomes to be zipped with their fitness
    parent2 <- selectionFun popFitnessTuple tournSize
    let parents = parent1:parent2:[] -- [Genome a]
    -- MUTATION
    parents <- mapM mutationFun parents
    -- CROSSOVER
    parents <- doCrossovers parents crossoverFun
    -- EVALUATION (FITNESS, obtain a list of (genome,fitness) pairs
    newPop <- evalFun opps parents
    return $ newPop

runnnEA :: Int -> Int ->
    Rand PureMT [(Genome Double, Int)] -> 
    ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
    ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
    Crossover Double -> 
    Mutation Double ->
    [Genome Double] -> 
    PureMT ->
    Rand PureMT [(Genome Double, Int)]
runnnEA 0 tournSize pop selectionFun evalFun crossoverFun mutationFun opps g = funn tournSize pop selectionFun evalFun crossoverFun mutationFun opps
runnnEA timesRun tournSize pop selectionFun evalFun crossoverFun mutationFun opps g = 
    return $ newGen ++ evalRand (runnnEA (timesRun-2) tournSize pop selectionFun evalFun crossoverFun mutationFun opps newg) g
    where
        (newGen, newg) = runRand (funn tournSize pop selectionFun evalFun crossoverFun mutationFun opps) g
        --newGen = evalRand (funn tournSize pop selectionFun evalFun crossoverFun mutationFun opps) g

-- runnnEA :: Int -> Int ->
--     Rand PureMT [(Genome Double, Int)] -> 
--     ([(Genome Double, Int)] -> Int -> Rand PureMT (Genome Double)) ->
--     ([Genome Double] -> [Genome Double] -> Rand PureMT [(Genome Double, Int)]) ->
--     Crossover Double -> 
--     Mutation Double ->
--     [Genome Double] -> 
--     PureMT ->
--     [(Genome Double, Int)]
-- runnnEA 0 tournSize pop selectionFun evalFun crossoverFun mutationFun opps g = []
-- runnnEA timesRun tournSize pop selectionFun evalFun crossoverFun mutationFun opps g = 
--     newGen ++ (runnnEA (timesRun-2) tournSize pop selectionFun evalFun crossoverFun mutationFun opps g)
--     where
--         newGen = evalRand (funn tournSize pop selectionFun evalFun crossoverFun mutationFun opps) g