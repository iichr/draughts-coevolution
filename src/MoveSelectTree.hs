module MoveSelectTree where

import Boardvector
import Data.List (minimumBy, maximumBy)
import Data.Ord (comparing)
import Data.Maybe (isJust)
import Evolution

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