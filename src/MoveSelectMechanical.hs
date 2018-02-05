module MoveSelectMechanical where

import Boardvector
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Vector as V
import System.Random

-- ******************************************
-- ************* JUMP SELECTION *************
-- ******************************************

-- Return an (origin, inbetween, destination) tuple
-- * Temporary solution : using a fixed seed "random" number generator for demonstration purposes
-- * move selection will not be this type of random in the future
-- avoid IO monad
selectJump :: [(Position, (Position, Position))] -> (Position, Position, Position)
selectJump xs = (fst x, snd $ snd x, fst $ snd x) 
    where
        randnum = randomR (0, length xs - 1) (mkStdGen 432)
        x = xs !! fst randnum


-- ******************************************
-- ******** SIMPLE MOVE SELECTION ***********
-- ******************************************

-- Return an origin, destination tuple
-- * Temporary solution : using a fixed seed "random" number generator for demonstration purposes
-- * move selection will not be this type of random in the future
-- avoid IO Monad
selectSimpleMove :: [(Position, Position)] -> (Position, Position)
selectSimpleMove xs =  x
    where
        randnum = randomR (0, length xs - 1) (mkStdGen 429834)
        x = xs !! fst randnum


-- ******************************************
-- ******** MOVE EXECUTION GENERIC **********
-- ******************************************

-- Given a game state return a new game state with the next player's turn
-- and the move of the previous player executed
-- if there are any jumps available only consider those
-- pass the list of availables to each auxiliary function requiring it for efficiency
performMoveAI :: GameState -> GameState
performMoveAI oldGameState
    | not . null $ availableJumps = performJump oldGameState (selectJump availableJumps)
    | not . null $ availableSimpleMoves  = performSimpleMove oldGameState (selectSimpleMove availableSimpleMoves)
    | otherwise = oldGameState
    where
        availableJumps = getJumps oldGameState
        availableSimpleMoves = getSimpleMoves oldGameState


aiNextState :: GameState -> IO GameState
aiNextState gs = return $ performMoveAI gs

