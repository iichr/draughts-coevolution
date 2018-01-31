module Boardvector where

import Data.List (intersperse, concat, foldl')
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Control.Monad
import System.Random

import Data.Vector.Unboxed (create, freeze)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as MUV

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
        show board ++ show player ++ "\n"

        
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
canMoveInto :: VectorBoard -> Position -> Bool
canMoveInto (VectorBoard b) dest@(row, col) = row<8 && row>=0 && col<8 && col>=0 && getSquare (VectorBoard b) dest == Just Empty

-- same as above, specifically for a jump move
-- deals with arguments passed in the form of (destination, position of piece inbetween)
-- for checking what figure is inbetween see jump
canJumpInto :: VectorBoard -> (Position, Position) -> Bool
canJumpInto (VectorBoard b) ((row,col), inbetween) = canMoveInto (VectorBoard b) (row,col)

-- A simple move consists of either:
-- moving an uncrowned piece one square FORWARD DIAGONALLY to an adjacent unoccupied dark square, or
-- moving a king piece kings one square IN ANY DIAGONAL DIRECTION
simpleMove :: VectorBoard -> Position -> [Position]
simpleMove (VectorBoard b) orig@(row, col)
        |Just (Tile White Man) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) whiteMoves
        |Just (Tile Black Man) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) blackMoves
        |Just (Tile _ King) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) kingMoves
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
                                notElem z [Just (Tile White Man), Just (Tile White King), Just Empty]
                                ]

        |Just (Tile Black Man) <- getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- blackZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween, 
                                notElem z [Just (Tile Black Man), Just (Tile Black King), Just Empty]
                                ]
                                                                                                        
        |Just (Tile White King) <- getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- kingZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween, 
                                notElem z [Just (Tile White Man), Just (Tile White King), Just Empty]
                                ]

        |Just (Tile Black King) <- getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- kingZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween, 
                                notElem z [Just (Tile Black Man), Just (Tile Black King), Just Empty]
                                ]
                                               
        |otherwise = []
    where
        -- mind the order inside for zip to work properly!
        whiteInbetween = [(row-1, col-1), (row-1, col+1)]
        blackInbetween = [(row+1, col-1), (row+1, col+1)]

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

-- SAFE FOR HUMAN PLAYERS - MOVES ARE VALIDATED
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


-- !!!!!!!!!!!!!!!!!!
-- ! JUMPS
-- !!!!!!!!!!!!!!!!!!

-- ! NOT SAFE FOR HUMANS - DOES NOT VERIFY MOVE VALIDITY
-- Players will be predetermined and will only have access to the list of available moves to choose from
-- hence a second check for move validity is redundant
performJump :: GameState -> (Position, Position, Position) -> GameState
performJump oldGameState@(GameState (VectorBoard b) player1) (orig, inbtwn, dest) = GameState newBoardJumped player2
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



-- Return an (origin, inbetween, destination) tuple
-- * Temporary solution : using a fixed seed "random" number generator for demonstration purposes
-- * move selection will not be this type of random in the future
-- avoid IO monad
selectJump :: [(Position, (Position, Position))] -> (Position, Position, Position)
selectJump xs = (fst x, snd $ snd x, fst $ snd x) 
    where
        randnum = randomR (0, length xs - 1) (mkStdGen 7839)
        x = xs !! fst randnum


-- !!!!!!!!!!!!!!!!!!!
-- ! SIMPLE MOVES
-- !!!!!!!!!!!!!!!!!!!

-- ! NOT SAFE FOR HUMANS - DOES NOT VERIFY MOVE VALIDITY
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

-- Return an origin, destination tuple
-- * Temporary solution : using a fixed seed "random" number generator for demonstration purposes
-- * move selection will not be this type of random in the future
-- avoid IO Monad
selectSimpleMove :: [(Position, Position)] -> (Position, Position)
selectSimpleMove xs =  x
    where
        randnum = randomR (0, length xs - 1) (mkStdGen 64572)
        x = xs !! fst randnum

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
won :: GameState -> Maybe Player
won gs@(GameState (VectorBoard b) player1) = undefined

-- ********************
-- ***** WEIGHTS ******
-- ********************

-- Construct an initial vector storing the weight of each board square
-- Unboxed for efficienct - consecutive memory slots without pointers.
-- TODO: actual values
makeWeightinit :: UV.Vector Weight
makeWeightinit = UV.create $ do
    MUV.replicate 64 (1.0 :: Double)

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
pieceVal (Tile Black Man) = -1.0
pieceVal (Tile Black King) = -5.0
pieceVal (Tile White Man) = 1.0
pieceVal (Tile White King) = 5.0
pieceVal Empty = 0.0

-- The sum of the board, i.e. for all squares the value of the piece times the corresponding weight of its position
-- from the weights vector; a positive result: Player White has an advantage, Black does not, 
-- and vice versa for a negative result
getSum :: GameState -> Double
getSum gs = foldr (+) 0.0 $ getVectortoSum gs 
        where    
            getVectortoSum :: GameState -> [Double]
            getVectortoSum gs@(GameState (VectorBoard b) _) = do
                row <- [0..7]
                col <- [0..7]
                let pos = (row, col)
                let square = fromJust $ getSquare (VectorBoard b) pos
                let i = convertPos2Index pos
                let weightsvector = makeWeightinit
                let weight = weightsvector UV.! i
                let w = pieceVal square * weight
                return w




