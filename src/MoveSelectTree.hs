module MoveSelectTree where

import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust)

import Control.Monad.Random
import System.Random.Mersenne.Pure64

import Utils
import DraughtsBoard
import GamePlay

-- Positive infinity
posInf :: Fractional a => a
posInf = 1/0

-- Negative infinity
negInf :: Fractional a => a
negInf = -1/0

-- Return the element of a list that minimises a function. In case of a tie,
-- return the element closest to the front of the list.
argMin :: Ord b => [a] -> (a -> b) -> a
argMin xs f = minimumBy (comparing f) xs

-- Return the element of the target list that maximises a function.
argMax :: (Ord b, Num b) => [a] -> (a -> b) -> a
argMax xs f = argMin xs (negate . f)
--argMax xs f = maximumBy (comparing f) xs


-- Whether the desired depth in the search has been reached
stopCondition :: Int -> Int -> Bool
stopCondition depth limit = depth == limit


-- ****************************************************************************
-- ************** ALPHA BETA DEPTH LIMITED SEARCH *****************************
-- ! ***********  WITH RANDOMNESS (OPTIONALLY PROVIDED)  **********************
-- ****************************************************************************

minValue' :: (Fractional a, Ord a) => a -> a -> Int -> Int -> GameState -> Genome Double -> (Genome Double -> GameState -> a) -> a
minValue' alpha beta depth limit state genome evaluator =
    if endOfGame || stopCondition depth limit
        then evaluator genome state
    else 
        fmin posInf beta (getSuccessiveStates state)
    where
        endOfGame = isJust (whoWon state)
        fmin val beta [] = val
        -- if a state with lower than alpha evaluation has been reached, stop looking any further
        fmin val beta (s:gss) = if val <= alpha then val else fmin newVal (min beta newVal) gss
            where
                newVal = min val (maxValue' alpha beta (depth+1) limit s genome evaluator)


maxValue' :: (Fractional a, Ord a) => a -> a -> Int -> Int -> GameState -> Genome Double -> (Genome Double -> GameState -> a) -> a
maxValue' alpha beta depth limit state genome evaluator =
    if endOfGame || stopCondition depth limit
        then evaluator genome state
    else 
        fmax negInf alpha (getSuccessiveStates state)
    where
        endOfGame = isJust (whoWon state)
        fmax val alpha [] = val
        fmax val alpha (s:gss) = if val >= beta then val else fmax newVal (max alpha newVal) gss 
            where
                newVal = max val (minValue' alpha beta (depth+1) limit s genome evaluator)

          

alphabetadepthlimneg' :: Double -> Double -> Int -> Int -> GameState -> Genome Double -> (Genome Double -> GameState -> Double) -> Rand PureMT GameState
alphabetadepthlimneg' alpha beta depth limit state genome evaluator = do
    -- join in place of concat
    -- apply minValue' recursively to all elements of the list of successive states
    -- generate random numbers the length of that list and sum them with the respective state to ensure diversity
    -- zip evaluation and state together
    let allStatesEvaluated = join $ [map (\s -> maxValue' alpha beta depth limit s genome evaluator) (getSuccessiveStates state)]
    -- l <- replicateM (length $ allStatesEvaluated) $ getRandomR ((0.0)::Double,(0.0))
    -- let res = zipWith (+) allStatesEvaluated l
    let res = allStatesEvaluated
    --let b = map ((,) r) (getSuccessiveStates state)
    let tupleEvalState = zip res (getSuccessiveStates state)
    -- up to here the return type is Rand PureMT [(Double, GameState)]
    randomWithProbability 0.2 tupleEvalState $ minimumBy (comparing fst)
    -- * OLD DETERMINISTC
    -- let b = minimumBy (comparing fst) tupleEvalState
    -- -- up to here the return type is Rand PureMT (Double, GameState)
    -- return $ snd b


alphabetadepthlim' :: Double -> Double -> Int -> Int -> GameState -> Genome Double -> (Genome Double -> GameState -> Double) -> Rand PureMT GameState
alphabetadepthlim' alpha beta depth limit state genome evaluator = do
    -- join in place of concat
    -- apply minValue' recursively to all elements of the list of successive states
    -- generate random numbers the length of that list and sum them with the respective state to ensure diversity
    -- zip evaluation and state together
    let allStatesEvaluated = join $ [map (\s -> minValue' alpha beta depth limit s genome evaluator) (getSuccessiveStates state)]
    --l <- replicateM (length $ allStatesEvaluated) $ getRandomR ((0.0)::Double,(0.0))
    -- let res = zipWith (+) allStatesEvaluated l
    let res = allStatesEvaluated
    --let b = map ((,) r) (getSuccessiveStates state)
    let tupleEvalState = zip res (getSuccessiveStates state)
    -- up to here the return type is Rand PureMT [(Double, GameState)]
    randomWithProbability 0.2 tupleEvalState $ maximumBy (comparing fst)
    -- * OLD DETERMINISTIC
    -- let b = maximumBy (comparing fst) tupleEvalState
    -- -- up to here the return type is Rand PureMT (Double, GameState)
    -- return $ snd b
   
 
-- * Choose a random move based on some probability p supplied.
-- * It takes three arguments: the probability p, a list of states and their evaluations, 
-- * a function that returns a gameState based on some criteria - maximumBy or minimumBy depending on the player
-- This latter function is only used when a non-random move is needed - standard deterministic behaviour
-- * Hinweis: set p to 0.0 to effectively choose a move deterministically
randomWithProbability :: Double -> [(Double, GameState)] -> ([(Double, GameState)] -> (Double, GameState)) -> Rand PureMT GameState
randomWithProbability p evalStateTuple deterministicMovePick = do
    -- get a random number in the range of 0.0 to 1.0
    t <- getRandomR (0.0, 1.0)
    -- with probability p pick a random state
    -- chance of t being less than p is p therefore the bigger the p, the higher the probability 
    -- the randomly chosen element t will be less than it
    -- evalStateTuple <- evalStateTup
    if t < p
        then do
            let maxIndex = (length evalStateTuple) - 1
            index <- getRandomR (0, maxIndex)
            let chosenTuple = evalStateTuple !! index
            return $ snd chosenTuple
        else do
            let chosenTup = deterministicMovePick evalStateTuple
            return $ snd chosenTup


-- *********************************************
-- ******* ALPHA BETA DEPTH LIMITED SEARCH *****
-- *********************************************  

-- Depth-limited alpha beta search using an evaluation function
-- Adapted from pseudocode in Stuart Russell, Peter Norvig - Artificial Intelligence - A Modern Approach 3rd Ed
-- Chapter 5, pp. 170 - 175
-- initial depth set to 0
alphabetadepthlim :: Int -> Genome Double -> (Genome Double -> GameState -> Double) -> GameState -> GameState
alphabetadepthlim limit genome evaluator gs@(GameState (VectorBoard b) player1) = 
    argMax allsuccessive (minValue negInf posInf 0)
        where
            allsuccessive = getSuccessiveStates gs

            minValue alpha beta depth state
                | endOfGame = evaluator genome state
                | stopCondition depth limit = evaluator genome state
                | otherwise = 
                    fmin posInf beta (getSuccessiveStates state)
                        where
                            endOfGame = isJust (whoWon state)
                            fmin val beta [] = val
                            fmin val beta (s:gss) = if val <= alpha then val else fmin newVal (min beta newVal) gss
                                where
                                    newVal = min val (maxValue alpha beta (depth+1) s)
            
            maxValue alpha beta depth state
                | endOfGame = evaluator genome state
                | stopCondition depth limit = evaluator genome state
                | otherwise = 
                    fmax negInf alpha (getSuccessiveStates state)
                        where
                            endOfGame = isJust (whoWon state)
                            fmax val alpha [] = val
                            fmax val alpha (s:gss) = if val >= beta then val else fmax newVal (max alpha newVal) gss 
                                where
                                    newVal = max val (minValue alpha beta (depth+1) s)

