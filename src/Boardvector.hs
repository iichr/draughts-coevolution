module Boardvector where

import Data.List (intersperse, concat, foldl')
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Control.Monad

import Data.Vector.Unboxed (create, freeze)
import qualified Data.Vector.Unboxed as UV
-- import qualified Data.Vector.Unboxed.Mutable as MUV
import System.Random
import Evolution

import Control.Monad.Random
import System.Random.Mersenne.Pure64

-- DATA TYPES

data Player = Black | White
    deriving (Eq, Show)

data Figure = Man | King
    deriving (Eq, Show)

data Square = Empty | Tile Player Figure
    deriving Eq

type Position = (Int, Int)

type Weight = Double

data VectorBoard = VectorBoard (V.Vector (V.Vector Square))
    deriving Eq

data GameState = GameState VectorBoard Player
    deriving Eq

instance Show Square where
    show Empty = "."
    show (Tile Black Man) = "b"
    show (Tile Black King) = "B"
    show (Tile White Man) = "w"
    show (Tile White King) = "W"

instance Show VectorBoard where
    show board = unlines ([colIndex] ++ boardStr) where
        -- display the column index at the top
        colIndex = concat (intersperse " | " $ (id " ") : [ [x] | x <- ['0'..'7'] ]) ++ " |"
        -- display each row along with its index
        boardStr = zipWith showRow [0..7] $ boardToList board
        -- HELPERS
        --  convert board to list of squares
        boardToList (VectorBoard b) = V.toList $ V.map V.toList b
        -- display a single board row
        showRow i sqs = concat (intersperse " | " $ (show i) : (map show sqs) ) ++ " |"

instance Show GameState where
    show (GameState board player) = 
        show board ++ show player

        
-- convert a position in a 2D array in the row, column format to a 1D index; zero-indexed
convertPos2Index :: Position -> Int
convertPos2Index (row, col) = row * 8 + col

initialBoard :: VectorBoard
-- each elem in the concat is of type V.Vector (V.Vector Square)
initialBoard = VectorBoard $ V.concat[V.replicate 1 (initialOddRow blackrow),
                        V.replicate 1 (initialEvenRow blackrow),
                        V.replicate 1 (initialOddRow blackrow),
                        V.replicate 2 emptyrow, 
                        V.replicate 1 (initialEvenRow whiterow),
                        V.replicate 1 (initialOddRow whiterow),
                        V.replicate 1 (initialEvenRow whiterow)]
                    where
                        blackrow = V.replicate 8 (Tile Black Man)
                        whiterow = V.replicate 8 (Tile White Man)
                        emptyrow = V.replicate 8 Empty
                        -- generate rows 1,3 and 7
                        initialOddRow vect = vect V.// [(i, Empty) | i <- [0..7], even i]
                        -- generate rows 2,6 and 8
                        initialEvenRow vect = vect V.// [(i, Empty) | i <- [0..7], odd i]


-- Get a square from the board given one and a position on the board
-- if one fails we want the whole evaluation to fail
-- hence using Maybe as a Monad (analogous to <<=)
getSquare :: VectorBoard -> Position -> Maybe Square
getSquare (VectorBoard b) (r,c) = do
    row <- b V.!? r
    row V.!? c

-- replace x-th vector of the board with (the replacement of y-th element of the x-th vector with a newfigure)
setSquare :: VectorBoard -> Square -> Position -> VectorBoard
setSquare (VectorBoard b) newfig (x,y) = VectorBoard $ b V.// [(x, b V.! x V.// [(y, newfig)])]

-- make an element a king when it reaches the appropriate row
kingify :: Square -> Square
kingify Empty = Empty
kingify (Tile player _) = Tile player King

-- should an element be kinged depending on the player and its current position, 
-- if so return the square which should be kinged
-- white promoted at 0
-- black at 7
shouldPromote :: Player -> Position -> Bool
shouldPromote White (row, _) = row == 0
shouldPromote Black (row, _) = row == 7

-- Does a given tile belong to a given player
whoseTile :: Maybe Square -> Player -> Bool
whoseTile (Just Empty) _ = False
whoseTile (Just(Tile player1 _)) player2 = player1 == player2
whoseTile Nothing _ = False

-- Get the opposite player of the one given
oppositeOf :: Player -> Player
oppositeOf White = Black
oppositeOf Black = White

-- check whether a given position is within the range of the board and EMPTY as well
canMoveIntoDest :: VectorBoard -> Position -> Bool
canMoveIntoDest (VectorBoard b) dest = getSquare (VectorBoard b) dest == Just Empty &&  getSquare (VectorBoard b) dest /= Nothing

-- A simple move consists of either:
-- moving an uncrowned piece one square FORWARD DIAGONALLY to an adjacent unoccupied dark square, or
-- moving a king piece kings one square IN ANY DIAGONAL DIRECTION
simpleMove :: VectorBoard -> Position -> [Position]
simpleMove (VectorBoard b) orig@(row, col)
        |Just (Tile White Man) <- getSquare (VectorBoard b) orig =
            filter (canMoveIntoDest (VectorBoard b)) whiteMoves
        |Just (Tile Black Man) <- getSquare (VectorBoard b) orig =
            filter (canMoveIntoDest (VectorBoard b)) blackMoves
        |Just (Tile _ King) <- getSquare (VectorBoard b) orig =
            filter (canMoveIntoDest (VectorBoard b)) kingMoves
        |otherwise = []
    where
        whiteMoves = [(row-1, col-1), (row-1, col+1)]
        blackMoves = [(row+1, col-1), (row+1, col+1)]
        kingMoves = whiteMoves ++ blackMoves

-- A jump move:
-- can be made over an opponent's piece only
-- check whether the destination to jump to is valid and Empty
-- check whether the position to be jumped to inbetween is neither Empty nor belonging to the same player
jump :: VectorBoard -> Position -> [(Position,Position)]
jump (VectorBoard b) orig@(row, col)
        |Just (Tile White Man) <- getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- whiteZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween,
                                notElem z [Just (Tile White Man), Just (Tile White King), Just Empty, Nothing],
                                let y = getSquare (VectorBoard b) dest,
                                elem y [Just Empty]
                                ]

        |Just (Tile Black Man) <- getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- blackZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween, 
                                notElem z [Just (Tile Black Man), Just (Tile Black King), Just Empty, Nothing],
                                let y = getSquare (VectorBoard b) dest,
                                elem y [Just Empty]
                                ]
                                                                                                        
        |Just (Tile White King) <- getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- kingZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween, 
                                notElem z [Just (Tile White Man), Just (Tile White King), Just Empty, Nothing],
                                let y = getSquare (VectorBoard b) dest,
                                elem y [Just Empty]
                                ]

        |Just (Tile Black King) <- getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- kingZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween, 
                                notElem z [Just (Tile Black Man), Just (Tile Black King), Just Empty, Nothing],
                                let y = getSquare (VectorBoard b) dest,
                                elem y [Just Empty]
                                ]
                                               
        |otherwise = []
    where
        -- mind the order inside for zip to work properly!
        -- apply filter if necessary: filter (\n -> (getSquare (VectorBoard b) n) /= Nothing)
        whiteInbetween = [(row-1, col-1), (row-1, col+1)]
        blackInbetween = [(row+1, col-1), (row+1, col+1)]
    
        -- OLD SOLUTION: filter (\n -> canMoveIntoDest (VectorBoard b) n)
        whiteJumps = [(row-2, col-2), (row-2, col+2)]
        blackJumps = [(row+2, col-2), (row+2, col+2)]

        -- pair jump with the position to be jumped inbetween for checking if a jump is allowed
        whiteZippedPath = zip whiteJumps whiteInbetween
        blackZippedPath = zip blackJumps blackInbetween
        kingZippedPath = whiteZippedPath ++ blackZippedPath

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

-- Given an origin and destination position tuple and a list of triplets of jump positions
-- in the form of (origin, (destination, inbetween)) find the tuple in the list and 
-- extract the position of the square inbetween the origin and destination if it exists
getInbetweenPosition :: (Position, Position) -> [(Position, (Position, Position))] -> Maybe Position
getInbetweenPosition _ [] = Nothing
getInbetweenPosition (orig, dest) ((o,(d,i)):ps) 
    | orig == o && dest == d = Just i
    | otherwise = getInbetweenPosition (orig, dest) ps

-- Check if a both elements of tuple of origin and destination positions are members of a list of triplets
elemTriplet :: (Position, Position) -> [(Position, (Position, Position))] -> Bool
elemTriplet _ [] = False
elemTriplet (x,y) ((k,(l,_)):ps)
    | x == k && y == l = True
    | otherwise = elemTriplet (x,y) ps


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


-- Convert a jump triple in the form of (origin, (destination, inbetween)) 
-- to (origin, inbetween, destination)
flattenJump :: (Position, (Position, Position)) -> (Position, Position, Position)
flattenJump (a,(b,c)) = (a,c,b)

-- ********************************
-- ***** WIN/END OF GAME **********
-- ********************************

-- Get the number of figures each player has on the board, with the result in the form of (Black, White)
figureCount :: VectorBoard -> (Int,Int)
-- fold on current element tuple and the rest of the list, matching on it
figureCount board = foldl' (\(b,w) r -> case r of 
                                            Just (Tile Black _) -> (b+1,w)
                                            Just (Tile White _) -> (b,w+1)
                                            _ -> (b,w)
                            ) (0,0) sqs where
   sqs = filter (\n -> n /= Just Empty) [getSquare board (row, col) | row <- [0..7], col <- [0..7]]

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


-- ********************
-- ***** WEIGHTS ******
-- ********************

-- Construct an initial vector storing the weight of each board square
-- Unboxed for efficienct - consecutive memory slots without pointers.
-- TODO: actual values
makeWeightinit :: UV.Vector Weight
-- makeWeightinit = UV.create $ do
--     MUV.replicate 64 (1.0 :: Double)
makeWeightinit = UV.replicate 64 (1.0 :: Double)

-- !NEW
makeWeightinitNEW :: IO (UV.Vector Weight)
makeWeightinitNEW = UV.replicateM 64 $ randomRIO (0.1 :: Double, 1.0 :: Double)

-- !NEW
makeWeightPopulationNEW :: V.Vector (IO (UV.Vector Weight))
makeWeightPopulationNEW = V.concat[V.replicate 100 makeWeightinitNEW]
-- display: sequence $ makeWeightPopulationNEW V.!? 47


-- Update a weight vector as a result of a run
-- Use mutability to increase efficiency as frequent changes will be made
-- TODO: implement
updateWeightMutably :: UV.Vector Weight -> GameState -> UV.Vector Weight
updateWeightMutably oldWeight gs@(GameState (VectorBoard b) player1) = undefined


-- ***************************
-- ***** WEIGHTED PIECES *****
-- ***************************

-- Assign to each piece a value, used in the evaluation of each board
-- positive for White
-- negative for Black
pieceVal :: Square -> Double
pieceVal (Tile Black Man) = 1.0
pieceVal (Tile Black King) = 1.3
pieceVal (Tile White Man) = -1.0
pieceVal (Tile White King) = -1.3
pieceVal Empty = 0.0



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

-- ***********************
-- ***** RUN GAME IO *****
-- ***********************

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
    

-- ********************
-- ***** EVOLUTION *****
-- ********************

getSum' :: Genome Double -> GameState -> Rand PureMT Double
getSum' g gs = do
    t <- getRandomR((-0.25)::Double, 0.25)
    let balance = t
    return $ (getSum g gs) + balance

-- The sum of the board, i.e. for all squares the value of the piece times the corresponding weight of its position
-- from the weights vector; a positive result: Player White has an advantage, Black does not, 
-- and vice versa for a negative result
getSum :: Genome Double -> GameState -> Double
getSum genome gs = (foldr (+) 0.0 $ getVectortoSum gs genome) +
                   1.2 * (fromIntegral jumpscount) + 0.7 * (fromIntegral simplemovescount)
--                   + (foldr (+) 0.0 $ getVectortoSumEdges gs genome)

        where 
            jumpscount = length $ getJumps gs
            simplemovescount = length $ getSimpleMoves gs
            
            
            getVectortoSum :: GameState -> Genome Double -> [Double]
            getVectortoSum gs@(GameState (VectorBoard b) _) genome = do
                row <- [0..7]
                col <- [0..7]
                let pos = (row, col)
                let square = fromJust $ getSquare (VectorBoard b) pos
                let i = convertPos2Index pos
                --let weightsvector = makeWeightinit
                let weight = genome !! i
                let w = (pieceVal square) * 1.7 * weight
                return w

            -- getVectortoSumEdges :: GameState -> Genome Double -> [Double]
            -- getVectortoSumEdges gs@(GameState (VectorBoard b) _) genome = do
            --     row <- [1,6]
            --     col <- [0..7]
            --     let pos = (row, col)
            --     let square = fromJust $ getSquare (VectorBoard b) pos
            --     let i = convertPos2Index pos
            --     --let weightsvector = makeWeightinit
            --     let weight = genome !! i
            --     let w = (pieceVal square) * 0.7 * weight
            --     return w

          
-- Get a tuple of the list of GameState and their respective sums
-- getSumforList :: [GameState] -> Genome Double -> [(GameState, Double)]
-- getSumforList xs genome = zip xs $ map (\x -> getSum genome x) xs
-- use map snd getSumforList to obtain the values only

-- ***************************
-- ***** RUN GAME NON IO *****
-- ***************************

playnonIO :: Enum a => Int -> Genome a -> Genome a -> (Genome a -> GameState -> GameState) -> (Genome a -> GameState -> GameState) -> GameState -> Int
playnonIO counter gen1 gen2 ai1 ai2 gs@(GameState (VectorBoard b) player1)
        | whoWon gs == Just Black = 1
        | whoWon gs == Just White = -1
        | otherwise = if (counter == 0)
                        then 0
                        else playnonIO (counter-1) gen1 gen2 ai1 ai2 (getNextState gs)
        where
            getNextState gs = if player1 == Black then (ai1 gen1 gs) else (ai2 gen2 gs)


playnonIOMonadicSecond :: Enum a => Int -> Genome a -> Genome a -> (Genome a -> GameState ->  Rand PureMT GameState) -> (Genome a -> GameState ->  Rand PureMT GameState) -> Rand PureMT GameState -> Rand PureMT Int
playnonIOMonadicSecond counter gen1 gen2 ai1 ai2 ms = do
            gs <- ms
            playnonIO' counter gen1 gen2 ai1 ai2 gs

            -- check gs
            -- where
            --     check gs@(GameState (VectorBoard b) player1)
            --         | whoWon gs == Just Black = getRandomR(1,1)
            --         | whoWon gs == Just White = getRandomR(-1,-1)
            --         | otherwise = if (counter == 0)
            --             then getRandomR(0,0)
            --             else playnonIOMonadicSecond (counter-1) gen1 gen2 ai1 ai2 (getNextState gs)
            --                 where
            --                     getNextState gs = if 
            --                         player1 == Black then (ai1 gen1 gs) else (ai2 gen2 gs)


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

-- ********************
-- ***** FOR TESTING PURPOSES- BORROWED FROM BOARDVECTORSPEC *****
-- ********************

generateRow :: [Int] -> Square -> V.Vector Square
generateRow ys sq = V.replicate 8 Empty V.// (\x -> [(z,sq) | z <- x]) ys

changeRow :: V.Vector Square -> [Int] -> Square -> V.Vector Square
changeRow vect ys sq = vect V.// (\x -> [(z,sq) | z <- x]) ys

fullRow :: [Int] -> Square -> [Int] -> Square -> V.Vector Square
fullRow xs s1 ys x2 = changeRow (generateRow xs s1) ys x2

badBoard :: VectorBoard
-- white's turn
badBoard = VectorBoard $ V.fromList [r0, r1, r2, r3, r4, r5, r6, r7] where
    b = Tile Black Man
    w = Tile White Man
    --bk = Tile Black King
    --wk = Tile White King
    --empty = V.replicate 8 Empty
    r0 = generateRow [1,3,5,7] b
    r1 = generateRow [0,2,4,6] b
    r2 = generateRow [1,3,7] b
    r3 = generateRow [6] b
    r4 = generateRow [5] w
    r5 = generateRow [0,2,6] w
    r6 = generateRow [1,3,5,7] w
    r7 =  generateRow [0,2,4,6] w

    --getJumps (GameState badBoard White)
    -- [((4,5),((2,7),(3,6)))] is WRONG

badBoard2 :: VectorBoard
-- black's turn
badBoard2 = VectorBoard $ V.fromList [r0, r1, empty, r3, r4, r5, r6, r7] where
    b = Tile Black Man
    w = Tile White Man
    empty = V.replicate 8 Empty

    r0 = generateRow [1,5,7] b
    r1 = generateRow [0,2,4,6] b
    --r2 = empty
    r3 = generateRow [0,2] b
    r4 = generateRow [5] b
    r5 = generateRow [0,4,6] w
    r6 = fullRow [1,3] w [5] b
    r7 =  generateRow [0,2,6] w

    --getJumps (GameState badBoard2 Black)
    --[((4,5),((6,7),(5,4)))] is wrong
    -- inbetween is wrong!!! FIXED. Issue was filtering