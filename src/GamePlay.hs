module GamePlay where

import Data.List (intersperse, concat, foldl')
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Control.Monad

import Data.Vector.Unboxed (create, freeze)
import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as MUV
import System.Random

import Control.Monad.Random
import System.Random.Mersenne.Pure64

import Utils
import DraughtsBoard


-- * Get all simple moves a player can perform given a GameState, with results
-- * listed in the form of [(ORIGIN position), (DESTINATION position)]
getSimpleMoves :: GameState -> [(Position, Position)]
getSimpleMoves (GameState (VectorBoard b) player) = do
    row <- [0..7]
    col <- [0..7]
    let src = (row, col)
    let square = getSquare (VectorBoard b) src
    let simpleMoves = simpleMove (VectorBoard b) src
    -- let jumps = jump (VectorBoard b) src
    if whoseTile square player
        then map ((,) src) $ simpleMoves
        -- ++ jumps
        else []

        
-- * Get all jumps a player can perform given a GameState, with results
-- * listed in the form of [(ORIGIN position), (DESTINATION position, position of square INBETWEEN to be jumped)]
getJumps :: GameState -> [(Position, (Position, Position))]
getJumps (GameState (VectorBoard b) player) = do
    row <- [0..7]
    col <- [0..7]
    let src = (row, col)
    let square = getSquare (VectorBoard b) src
    let jumps = jump (VectorBoard b) src
    if whoseTile square player
        then map ((,) src) jumps
        else []



-- Given a gamestate get all possible simple moves and jumps emanating from it
-- either jumps (with precedence) or simple moves at a given time as per the rules of Draughts
getSuccessiveStates :: GameState -> [GameState]
getSuccessiveStates gs 
    | not . null $ availableJumps = map ((\ js -> performJump gs js) . flattenJump) availableJumps
    | not . null $ availableSimpleMoves  = map (performSimpleMove gs) availableSimpleMoves
    | otherwise = [gs]
    where
        availableJumps = getJumps gs
        availableSimpleMoves = getSimpleMoves gs

-- **********************************************************************
-- ******************************* MOVE EXECUTION ***********************
-- ***! SUITABLE FOR HUMAN PLAYERS AS MOVES ARE CHECKED FOR VALIDITY !***
-- **********************************************************************

-- Execute a move to a location given a source position and a gameState
-- returning a new gameState accepting the opposing player's turn
performMove :: GameState -> Position -> Position -> GameState
performMove oldGameState@(GameState (VectorBoard b) player1) orig dest
    | isJust inbetween = GameState newBoardJumped player2
    | (orig, dest) `elem` availableSimpleMoves = GameState newBoardSimple player2
    | otherwise = oldGameState
    where
        availableJumps = getJumps oldGameState
        availableSimpleMoves = getSimpleMoves oldGameState
        -- * JUMPS: get the coords of the position inbetween
        inbetween = getInbetweenPosition (orig, dest) availableJumps
        
        -- COMMON BEGIN
        player2 = oppositeOf player1
        -- get the figure at the original position
        fig = fromJust $ getSquare (VectorBoard b) orig
        -- remove that figure and return a board without it
        boardFigRemovedPlayer = setSquare (VectorBoard b) Empty orig
        -- check destination position for promotion, true if yes
        promotion = shouldPromote player1 dest
        -- COMMON END

        -- JUMPS: get the figure inbetween from the board with the player's original piece already removed
        -- figinbetween = fromJust $ getSquare boardFigRemovedPlayer (fromJust inbetween)
        -- * JUMPS: board bar the figure inbetween
        boardInbetweenRemoved = setSquare boardFigRemovedPlayer Empty (fromJust inbetween)
        
        newBoardSimple =
            if promotion
                -- add promoted figure to the newBoard destination position
                then setSquare boardFigRemovedPlayer (kingify fig) dest
                else setSquare boardFigRemovedPlayer fig dest
        
        -- * JUMPS: new board to be returned
        newBoardJumped =
            if promotion
                -- add promoted to the destination position
                then setSquare boardInbetweenRemoved (kingify fig) dest
                else setSquare boardInbetweenRemoved fig dest


-- *********************************
-- ******** MOVE EXECUTION *********
-- *****! ONLY FOR AI PLAYERS !*****
-- *********************************

-- Players will be predetermined and will only have access to the list of available moves to choose from
-- hence a second check for move validity is redundant
performSimpleMove :: GameState -> (Position, Position) -> GameState
performSimpleMove oldGameState@(GameState (VectorBoard b) player1) (orig, dest) = GameState newBoardSimple player2
    where
         player2 = oppositeOf player1
         -- get the figure at the original position
         fig = fromJust $ getSquare (VectorBoard b) orig
         -- remove that figure and return a board without it
         boardFigRemovedPlayer = setSquare (VectorBoard b) Empty orig
         -- check destination position for promotion, true if yes
         promotion = shouldPromote player1 dest
         -- return the new board
         newBoardSimple =
            if promotion
                -- add promoted figure to the newBoard destination position
                then setSquare boardFigRemovedPlayer (kingify fig) dest
                else setSquare boardFigRemovedPlayer fig dest


-- Players will be predetermined and will only have access to the list of available moves to choose from
-- hence a second check for move validity is redundant
performJump :: GameState -> (Position, Position, Position) -> GameState
performJump oldGameState@(GameState (VectorBoard b) player1) (orig, inbtwn, dest) = gameState
    where
        player2 = oppositeOf player1
        -- get the figure at the original position
        fig = fromJust $ getSquare (VectorBoard b) orig
        -- remove that figure and return a board without it
        boardFigRemovedPlayer = setSquare (VectorBoard b) Empty orig
        -- check destination position for promotion, true if yes
        promotion = shouldPromote player1 dest
        
        -- * REMOVE THE FIGURE AT THE INBETWEEN POSITION
        -- get the figure inbetween from the board with the player's original piece already removed
        -- figinbetween = fromJust $ getSquare boardFigRemovedPlayer inbtwn
        -- return a board bar the figure inbetween
        boardInbetweenRemoved = setSquare boardFigRemovedPlayer Empty inbtwn

        -- return the new board
        newBoardJumped =
            if promotion
                -- add promoted to the destination position
                then setSquare boardInbetweenRemoved (kingify fig) dest
                else setSquare boardInbetweenRemoved fig dest

        -- IF ANY MORE AVAILABLE JUMPS FROM THIS POSITION ARE AVAIILABLE
        -- unless the element was kinged (as per the rules)
        -- jump outputs 
        multipleJumps = jump newBoardJumped dest
        gameState
            | (not $ null $ multipleJumps) && not promotion = GameState newBoardJumped player1
            | otherwise = GameState newBoardJumped player2


-- ********************************
-- ***** WIN/END OF GAME **********
-- ********************************

-- Has the game been won by anyone yet, if not return Nothing, else - the winner
-- getting the available moves first is inefficient, first check the number of tiles
whoWon :: GameState -> Maybe Player
whoWon gs@(GameState (VectorBoard b) player1) = winner (figureCount (VectorBoard b))
    where
        availableJumps = getJumps gs
        availableSimpleMoves = getSimpleMoves gs
        winner :: (Int,Int) -> Maybe Player
        winner (0,_) = Just White
        winner (_,0) = Just Black
        winner _ = if null availableSimpleMoves && null availableJumps 
                   then Just $ oppositeOf player1 
                   else Nothing


-- ********************************************************************************************
-- ***************** GAME PLAYING FUNCTIONS ***************************************************
-- ********************************************************************************************

-- *************************************************
-- ******* IO **************************************
-- *************************************************

play :: Enum a => Int -> Genome a -> Genome a -> (Genome a -> GameState -> IO GameState) -> (Genome a -> GameState -> IO GameState) -> GameState -> IO ()
play counter gen1 gen2 ai1 ai2 gs@(GameState (VectorBoard b) player1) = do
    putStrLn (show gs ++ "| moves left " ++ show counter ++ "\n" )
    -- "| Board sum " ++  show boardSum ++ "\n" )
    -- ++ "Next state values |" ++ show _allvals ++ "\n")
    -- play until there is a winner
    case whoWon gs of
        Just player -> putStrLn $ show player ++ " HAS WON."
        Nothing -> do
            nextstate <- getNextState gs
            if (counter == 0)
                then putStrLn $ "it's a draw"
                else (play (counter-1) gen1 gen2 ai1 ai2 nextstate)
    where
        getNextState gs = if player1 == Black then (ai1 gen1 gs) else (ai2 gen2 gs)
        --boardSum = getSum gs
        -- _allvals = map snd $ getSumforList $ getSuccessiveStates gs
    

-- ***********************************************
-- ********* NON IO ******************************
-- ***********************************************

-- *************************
-- ***** NO RANDMONESS *****
-- *************************

playnonIO :: Enum a => Int -> Genome a -> Genome a -> (Genome a -> GameState -> GameState) -> (Genome a -> GameState -> GameState) -> GameState -> Int
playnonIO counter gen1 gen2 ai1 ai2 gs@(GameState (VectorBoard b) player1)
        | whoWon gs == Just Black = 1
        | whoWon gs == Just White = -1
        | otherwise = if (counter == 0)
                        then 0
                        else playnonIO (counter-1) gen1 gen2 ai1 ai2 (getNextState gs)
        where
            getNextState gs = if player1 == Black then (ai1 gen1 gs) else (ai2 gen2 gs)


-- ***************************
-- ***** WITH RANDMONESS *****
-- ***************************


playnonIOMonadicSecond :: Enum a => Int -> Genome a -> Genome a -> (Genome a -> GameState ->  Rand PureMT GameState) -> (Genome a -> GameState ->  Rand PureMT GameState) -> Rand PureMT GameState -> Rand PureMT Int
playnonIOMonadicSecond counter gen1 gen2 ai1 ai2 ms = do
            gs <- ms
            playnonIO' counter gen1 gen2 ai1 ai2 gs


playnonIO' :: Enum a => Int -> Genome a -> Genome a -> (Genome a -> GameState ->  Rand PureMT GameState) -> (Genome a -> GameState ->  Rand PureMT GameState) -> GameState -> Rand PureMT Int
playnonIO' counter gen1 gen2 ai1 ai2 gs@(GameState (VectorBoard b) player1)
        | whoWon gs == Just Black = getRandomR(1,1)
        | whoWon gs == Just White = getRandomR(-1,-1)
        | otherwise = if (counter == 0)
                        then getRandomR(0,0)
                        else playnonIOMonadicSecond (counter-1) gen1 gen2 ai1 ai2 (getNextState gs)
        where
            getNextState gs = if player1 == Black then (ai1 gen1 gs) else (ai2 gen2 gs)
            -- each statement in the brackets produces a Rand PureMT GameState as opposed to just a non-monadic one
            -- getNextState gs = do
            --     if player1 == Black 
            --         then do
            --             k <- (ai1 gen1 gs) 
            --             return k
            --         else do
            --             z <- (ai2 gen2 gs)
            --             return z