module DraughtsBoard where

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

-- Get a square from the board given one and a position on the board
getSquare :: VectorBoard -> Position -> Square
getSquare (VectorBoard b) (r,c) = V.unsafeIndex (V.unsafeIndex b r)  c


-- Get a square from the board given one and a position on the board
-- if one fails we want the whole evaluation to fail
-- hence using Maybe as a Monad (analogous to <<=)
getSquareSafe :: VectorBoard -> Position -> Maybe Square
getSquareSafe (VectorBoard b) (r,c) = do
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
-- White promoted at 0
-- Black at 7
shouldPromote :: Player -> Position -> Bool
shouldPromote White (row, _) = row == 0
shouldPromote Black (row, _) = row == 7

-- Does a given tile belong to a given player
whoseTile :: Square -> Player -> Bool
whoseTile (Tile player1 _) player2 = player1 == player2
whoseTile _ _ = False

-- check whether a given position is within the range of the board and EMPTY as well
canMoveIntoDest :: VectorBoard -> Position -> Bool
canMoveIntoDest (VectorBoard b) (r,c)
    | r > 7 || c > 7 || r < 0 || c < 0 = False
    | otherwise = getSquare (VectorBoard b) (r,c) == Empty

-- Check if a position is within the allowed range [(0,0), [7,7]]
isValidPosition :: Position -> Bool
isValidPosition (r,c)
    | r > 7 || c > 7 || r < 0 || c < 0 = False
    | otherwise = True

-- Check if a tuple of positions is valid
isValidTupleOfPositions :: (Position, Position) -> Bool
isValidTupleOfPositions (pos1, pos2) = isValidPosition pos1 && isValidPosition pos2


-- Get the number of figures each player has on the board, with the result in the form of (Black, White)
figureCount :: VectorBoard -> (Int,Int)
-- fold on current element tuple and the rest of the list, matching on it
figureCount board = foldl' (\(b,w) r -> case r of 
                                            (Tile Black _) -> (b+1,w)
                                            (Tile White _) -> (b,w+1)
                                            _ -> (b,w)
                            ) (0,0) sqs where
   sqs = [getSquare board pos | pos <- allPositionsInOrder]


-- A simple move consists of either:
-- moving an uncrowned piece one square FORWARD DIAGONALLY to an adjacent unoccupied dark square, or
-- moving a king piece kings one square IN ANY DIAGONAL DIRECTION
simpleMove :: VectorBoard -> Position -> [Position]
simpleMove (VectorBoard b) orig@(row, col)
        |(Tile White Man) == getSquare (VectorBoard b) orig =
            filter (canMoveIntoDest (VectorBoard b)) whiteMoves
        |(Tile Black Man) == getSquare (VectorBoard b) orig =
            filter (canMoveIntoDest (VectorBoard b)) blackMoves
        |(Tile Black King) == getSquare (VectorBoard b) orig =
            filter (canMoveIntoDest (VectorBoard b)) kingMoves
        |(Tile White King) == getSquare (VectorBoard b) orig =
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
        |(Tile White Man) == getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- whiteZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween,
                                notElem z [(Tile White Man), (Tile White King), Empty],
                                let y = getSquare (VectorBoard b) dest,
                                elem y [Empty]
                                ]

        |(Tile Black Man) == getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- blackZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween, 
                                notElem z [(Tile Black Man), (Tile Black King), Empty],
                                let y = getSquare (VectorBoard b) dest,
                                elem y [Empty]
                                ]
                                                                                                        
        |(Tile White King) == getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- kingZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween, 
                                notElem z [(Tile White Man), (Tile White King), Empty],
                                let y = getSquare (VectorBoard b) dest,
                                elem y [Empty]
                                ]

        |(Tile Black King) == getSquare (VectorBoard b) orig =
            [(dest,inbetween) | (dest,inbetween) <- kingZippedPath, 
                                let z = getSquare (VectorBoard b) inbetween, 
                                notElem z [(Tile Black Man),(Tile Black King), Empty],
                                let y = getSquare (VectorBoard b) dest,
                                elem y [Empty]
                                ]
                                               
        |otherwise = []
    where
        -- Mind the order inside for zip to work properly!
        -- SAFE VERSION: apply filter: filter (\n -> (getSquare (VectorBoard b) n) /= Nothing)
        whiteInbetween = [(row-1, col-1), (row-1, col+1)]
        blackInbetween = [(row+1, col-1), (row+1, col+1)]
    
        whiteJumps = [(row-2, col-2), (row-2, col+2)]
        blackJumps = [(row+2, col-2), (row+2, col+2)]

        -- pair jump with the position to be jumped inbetween for checking if a jump is allowed
        whiteZippedPath = filter (isValidTupleOfPositions) (zip whiteJumps whiteInbetween)
        blackZippedPath = filter (isValidTupleOfPositions) (zip blackJumps blackInbetween)
        kingZippedPath = whiteZippedPath ++ blackZippedPath
