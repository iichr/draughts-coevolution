module Boardvector where

import Data.List (intersperse, concat)
import Data.Maybe (fromMaybe, fromJust)
import qualified Data.Vector as V
import Control.Monad

-- DATA TYPES

data Player = Black | White
    deriving (Eq, Show)

data Figure = Man | King
    deriving (Eq, Show)

data Square = Empty | Tile Player Figure
    deriving Eq

type Position = (Int, Int)

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
shouldPromote White (row ,col) = row == 0
shouldPromote Black (row, col) = row == 7

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
jump :: VectorBoard -> Position -> [Position]
jump (VectorBoard b) orig@(row, col)
        |Just (Tile White Man) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) [dest | (dest,inbetween) <- whiteZippedPath, 
                                                    let z = getSquare (VectorBoard b) inbetween, 
                                                    notElem z [Just (Tile White Man), Just (Tile White King), Just Empty] ]

        |Just (Tile Black Man) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) [x | (x,y) <- blackZippedPath, 
                                                    let z = getSquare (VectorBoard b) y, 
                                                    notElem z [Just (Tile Black Man), Just (Tile Black King), Just Empty] ]
                                                                                                        
        |Just (Tile White King) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) [x | (x,y) <- kingZippedPath, 
                                                    let z = getSquare (VectorBoard b) y, 
                                                    notElem z [Just (Tile White Man), Just (Tile White King), Just Empty] ]

        |Just (Tile Black King) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) [x | (x,y) <- kingZippedPath, 
                                                    let z = getSquare (VectorBoard b) y, 
                                                    notElem z [Just (Tile Black Man), Just (Tile Black King), Just Empty] ]
                                                    
        |otherwise = []
    where
        -- mind the order for zip to work properly
        whiteInbetween = [(row-1, col-1), (row-1, col+1)]
        blackInbetween = [(row+1, col-1), (row+1, col+1)]

        whiteJumps = [(row-2, col-2), (row-2, col+2)]
        blackJumps = [(row+2, col-2), (row+2, col+2)]
        kingJumps = whiteJumps ++ blackJumps

        -- pair jump with the position to be jumped inbetween for checking
        whiteZippedPath = zip whiteJumps whiteInbetween
        blackZippedPath = zip blackJumps blackInbetween
        kingZippedPath = whiteZippedPath ++ blackZippedPath

-- * Get all moves a player can perform given a GameState, with results
-- * listed in the form of [(source position), (destination position)]
-- * alternative implementation VectorBoard -> Player -> etc
getPlayerMoves :: GameState -> [(Position, Position)]
getPlayerMoves (GameState (VectorBoard b) player) = do
    row <- [0..7]
    col <- [0..7]
    let src = (row, col)
    let square = getSquare (VectorBoard b) src
    let simpleMoves = simpleMove (VectorBoard b) src
    let jumps = jump(VectorBoard b) src
    if whoseTile square player
        then map ((,) src) $ simpleMoves ++ jumps
        else []

-- Execute a move to a location given a source position and a gameState
-- returning a new gameState accepting the opposing player's turn
--
-- TODO jump moves need to remove the element being jumped over
-- TODO if availableMoves is [] then the opposing player has won
performMove :: GameState -> Position -> Position -> GameState
performMove oldGameState@(GameState (VectorBoard b) player1) orig dest
    | (orig, dest) `elem` availableMoves = GameState newBoard player2
    | otherwise = oldGameState
    where
        availableMoves = getPlayerMoves oldGameState
        player2 = oppositeOf player1
        -- get the figure at the original position
        fig = fromJust $ getSquare (VectorBoard b) orig
        -- remove that figure and return a board without it
        boardFigRemovedPlayer = setSquare (VectorBoard b) Empty orig
        -- check destination position for promotion, true if yes
        promotion = shouldPromote player1 dest
        newBoard =
            if promotion
                -- add promoted figure to the newBoard destination position
                then setSquare boardFigRemovedPlayer (kingify fig) dest
                else setSquare boardFigRemovedPlayer fig dest

