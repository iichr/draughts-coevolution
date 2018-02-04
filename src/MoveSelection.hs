module MoveSelection where

import Boardvector

-- from (origin, (destination, inbetween)) to (origin, inbetween, destination)
flattenJump :: (Position, (Position, Position)) -> (Position, Position, Position)
flattenJump (a,(b,c)) = (a,c,b)

-- given a gamestate get all possible simple moves and jumps from it
-- run all of them, gathering the resulting gamestates + the src poisiton in a tuple
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




