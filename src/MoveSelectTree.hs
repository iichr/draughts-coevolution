module MoveSelectTree where

import Boardvector
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust)
import Evolution

import Control.Monad.Random
import System.Random.Mersenne.Pure64

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

-- argMin' :: Ord b => Rand PureMT [a] -> (a -> b) -> Rand PureMT a
-- argMin' xs f = do
--     ys <- xs
--     return $ minimumBy (comparing f) ys

-- Whether the desired depth in the search has been reached
stopCondition :: Int -> Int -> Bool
stopCondition depth limit = depth == limit

-- *********************************************
-- ******* ALPHA BETA DEPTH LIMITED SEARCH *****
-- ! ***********  WITH RANDOMNESS   ************
-- *********************************************

minValue' :: (Fractional a, Ord a) => a -> a -> Int -> Int -> GameState -> Genome Double -> (Genome Double -> GameState -> a) -> a
minValue' alpha beta depth limit state genome evaluator =
    if endOfGame || stopCondition depth limit
        then evaluator genome state
    else 
        fmin posInf beta (getSuccessiveStates state)
    where
        endOfGame = isJust (whoWon state)
        fmin val beta [] = val
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
    let allStatesEvaluated = join $ [map (\s -> minValue' alpha beta depth limit s genome evaluator) (getSuccessiveStates state)]
    l <- replicateM (length $ allStatesEvaluated) $ getRandomR ((-0.35)::Double,(0.35))
    let res = zipWith (+) allStatesEvaluated l
    --let b = map ((,) r) (getSuccessiveStates state)
    let tupleEvalState = zip res (getSuccessiveStates state)
    -- up to here the return type is Rand PureMT [(Double, GameState)]
    let b = minimumBy (comparing fst) tupleEvalState
    -- up to here the return type is Rand PureMT (Double, GameState)
    return $ snd b


alphabetadepthlim' :: Double -> Double -> Int -> Int -> GameState -> Genome Double -> (Genome Double -> GameState -> Double) -> Rand PureMT GameState
alphabetadepthlim' alpha beta depth limit state genome evaluator = do
    -- join in place of concat
    -- apply minValue' recursively to all elements of the list of successive states
    -- generate random numbers the length of that list and sum them with the respective state to ensure diversity
    -- zip evaluation and state together
    let allStatesEvaluated = join $ [map (\s -> minValue' alpha beta depth limit s genome evaluator) (getSuccessiveStates state)]
    l <- replicateM (length $ allStatesEvaluated) $ getRandomR ((-0.35)::Double,(0.35))
    let res = zipWith (+) allStatesEvaluated l
    --let b = map ((,) r) (getSuccessiveStates state)
    let tupleEvalState = zip res (getSuccessiveStates state)
    -- up to here the return type is Rand PureMT [(Double, GameState)]
    let b = maximumBy (comparing fst) tupleEvalState
    -- up to here the return type is Rand PureMT (Double, GameState)
    return $ snd b
   

performMoveAIalphabeta0PlyNonIO' :: Genome Double -> GameState -> Rand PureMT GameState
performMoveAIalphabeta0PlyNonIO' genome gs = alphabetadepthlim' negInf posInf 0 0 gs genome getSum    

performMoveAIalphabeta4PlyNonIOneg' :: Genome Double -> GameState -> Rand PureMT GameState
performMoveAIalphabeta4PlyNonIOneg' genome gs = alphabetadepthlimneg' negInf posInf 0 4 gs genome getSum

performMoveAIalphabeta0PlyNonIOneg' :: Genome Double -> GameState -> Rand PureMT GameState
performMoveAIalphabeta0PlyNonIOneg' genome gs = alphabetadepthlimneg' negInf posInf 0 0 gs genome getSum 

performMoveAIalphabeta4PlyNonIO' :: Genome Double -> GameState -> Rand PureMT GameState
performMoveAIalphabeta4PlyNonIO' genome gs = alphabetadepthlim' negInf posInf 0 4 gs genome getSum


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


minimaxdepthlim :: Int -> Genome Double -> (Genome Double -> GameState -> Double) -> GameState -> GameState
minimaxdepthlim limit genome evaluator gs@(GameState (VectorBoard b) player1) = 
    argMax succs (minValue 0)
    where
        succs  = getSuccessiveStates gs
        
        minValue depth state
            | endOfGame = evaluator genome state
            | stopCondition depth limit = evaluator genome state
            | otherwise =
                minimum [ maxValue (1+depth) s | s <- getSuccessiveStates state ]
                where
                    endOfGame = isJust (whoWon state)

        maxValue depth state
            | endOfGame = evaluator genome state
            | stopCondition depth limit = evaluator genome state
            | otherwise = 
                maximum [ minValue (1+depth) s | s <- getSuccessiveStates state ]
                where
                    endOfGame = isJust (whoWon state)

-- ******************************************
-- * IO PLAYERS, use with PLAY function *****
-- ******************************************

performMoveAIalphabeta0Ply :: Genome Double -> GameState -> IO GameState
performMoveAIalphabeta0Ply genome gs = return $ alphabetadepthlim 0 genome getSum gs

performMoveAIalphabeta3Ply :: Genome Double -> GameState -> IO GameState
performMoveAIalphabeta3Ply genome gs = return $ alphabetadepthlim 3 genome getSum gs

performMoveAIalphabeta4Ply :: Genome Double ->  GameState -> IO GameState
performMoveAIalphabeta4Ply genome gs = return $ alphabetadepthlim 4 genome getSum gs

performMoveAIalphabeta5Ply :: Genome Double ->  GameState -> IO GameState
performMoveAIalphabeta5Ply genome gs = return $ alphabetadepthlim 5 genome getSum gs

performMoveAIalphabeta6Ply :: Genome Double ->  GameState -> IO GameState
performMoveAIalphabeta6Ply genome gs = return $ alphabetadepthlim 6 genome getSum gs 

-- ******************************************
-- * MINIMAX IO PLAYERS, use with PLAY function *****
-- ******************************************

performMoveMinimax3Ply :: Genome Double -> GameState -> IO GameState
performMoveMinimax3Ply genome gs = return $ minimaxdepthlim 3 genome getSum gs

performMoveMinimax4Ply :: Genome Double ->  GameState -> IO GameState
performMoveMinimax4Ply genome gs = return $ minimaxdepthlim 4 genome getSum gs

-- ***************************************************
-- * NON-IO PLAYERS, use with PLAYNONIO function *****
-- ***************************************************

performMoveAIalphabeta0PlyNonIO :: Genome Double -> GameState -> GameState
performMoveAIalphabeta0PlyNonIO genome gs = alphabetadepthlim 0 genome getSum gs

performMoveAIalphabeta3PlyNonIO :: Genome Double -> GameState -> GameState
performMoveAIalphabeta3PlyNonIO genome gs = alphabetadepthlim 3 genome getSum gs

performMoveAIalphabeta4PlyNonIO :: Genome Double -> GameState -> GameState
performMoveAIalphabeta4PlyNonIO genome gs = alphabetadepthlim 4 genome getSum gs

performMoveAIalphabeta5PlyNonIO :: Genome Double ->  GameState -> GameState
performMoveAIalphabeta5PlyNonIO genome gs = alphabetadepthlim 5 genome getSum gs

performMoveAIalphabeta6PlyNonIO :: Genome Double ->  GameState -> GameState
performMoveAIalphabeta6PlyNonIO genome gs = alphabetadepthlim 6 genome getSum gs 