module MoveSelectTree where

import Boardvector
import Data.List (minimumBy)
import Data.Ord (comparing)

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


-- Convert a jump triple in the form of (origin, (destination, inbetween)) 
-- to (origin, inbetween, destination)
flattenJump :: (Position, (Position, Position)) -> (Position, Position, Position)
flattenJump (a,(b,c)) = (a,c,b)

-- Given a gamestate get all possible simple moves and jumps emanating from it
-- either jumps (with precedence) or simple moves at a given time as per the rules of Draughts
getSuccessiveStates :: GameState -> [GameState]
getSuccessiveStates gs 
    | not . null $ availableJumps = map (\js -> performJump gs js) (map flattenJump availableJumps)
    | not . null $ availableSimpleMoves  = map (\ms -> performSimpleMove gs ms) availableSimpleMoves
    | otherwise = [gs]
    where
        availableJumps = getJumps gs
        availableSimpleMoves = getSimpleMoves gs

-- Get a tuple of the list of GameState and their respective sums
getSumforList :: [GameState] -> [(GameState, Double)]
getSumforList xs = zip xs $ map getSum xs

-- Whether the desired depth in the search has been reached
stopCondition :: Int -> Int -> Bool
stopCondition depth limit = depth == limit

-- Depth-limited alpha beta search using an evaluation function 
-- Adapted from pseudocode in Stuart Russell, Peter Norvig - Artificial Intelligence - A Modern Approach 3rd Ed
-- Chapter 5, pp. 170 - 175
-- initial depth set to 0
alphabetadepthlim :: Int -> (GameState -> Double) -> GameState -> GameState
alphabetadepthlim limit evaluator gs@(GameState (VectorBoard b) player1) = 
    argMax allsuccessive (minValue negInf posInf 0)
        where
            allsuccessive = getSuccessiveStates gs

            minValue alpha beta depth state
                | endOfGame = evaluator state
                | stopCondition depth limit = evaluator state
                | otherwise = 
                    fmin posInf beta (getSuccessiveStates state)
                        where
                            endOfGame = whoWon state /= Nothing
                            fmin val beta [] = val
                            fmin val beta (s:gss) = if val <= alpha then val else fmin newVal (min beta newVal) gss
                                where
                                    newVal = min val (maxValue alpha beta (depth+1) s)
            
            maxValue alpha beta depth state
                | endOfGame = evaluator state
                | stopCondition depth limit = evaluator state
                | otherwise = 
                    fmax negInf alpha (getSuccessiveStates state)
                        where
                            endOfGame = whoWon state /= Nothing
                            fmax val alpha [] = val
                            fmax val alpha (s:gss) = if val >= beta then val else fmax newVal (max alpha newVal) gss 
                                where
                                    newVal = max val (minValue alpha beta (depth+1) s)


performMoveAIalphabeta3Ply ::  GameState -> IO GameState
performMoveAIalphabeta3Ply gs = return $ alphabetadepthlim 3 getSum gs

performMoveAIalphabeta5Ply ::  GameState -> IO GameState
performMoveAIalphabeta5Ply gs = return $ alphabetadepthlim 5 getSum gs

performMoveAIalphabeta6Ply ::  GameState -> IO GameState
performMoveAIalphabeta6Ply gs = return $ alphabetadepthlim 6 getSum gs
