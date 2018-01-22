module Boardvector where

import Data.List (intersperse, concat)
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Control.Monad
import Board

data VectorBoard = VectorBoard (V.Vector (V.Vector Square))
    deriving Eq

-- convert a board to a list of squares
boardToList :: VectorBoard -> [[Square]]
boardToList (VectorBoard b) = V.toList $ V.map V.toList b

-- HELPER - display a single row of the board
showRow :: Integer -> [Square] -> String
showRow i sqs = (concat $ intersperse " | " $ (show i) : (map show sqs) ) ++ " |"

instance Show VectorBoard where
    show board = unlines ([colIndex] ++ (boardStr)) where
        -- display the column index at the top
        colIndex = (concat $ intersperse " | " $ (id " ") : [ [x] | x <- ['0'..'7'] ]) ++ " |"
        -- display each row along with its index
        boardStr = zipWith showRow ([0..7]) $ boardToList board
    
-- 1 and 3, 7(white)
initialOddRow :: V.Vector Square -> V.Vector Square
initialOddRow vect = vect V.// [(i, Empty) | i <- [0..7], even i]

-- 2, 6 and 8(white)
-- replace the vector element at position i by Empty
initialEvenRow :: V.Vector Square -> V.Vector Square
initialEvenRow vect = vect V.// [(i, Empty) | i <- [0..7], odd i]

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
                            emptyrow = V.replicate 8 (Empty)

-- Get a square from the board given one and a position
-- 21 Jan changed to safe-indexing !? instead of ! 
-- strive for total function
getSquare :: VectorBoard -> Position -> Maybe Square
-- hacky way
-- getSquare (VectorBoard b) (r,c) = fromMaybe Empty ((fromMaybe (V.replicate 8 (Empty)) (b V.!? r)) V.!? c)
-- using Maybe as a Monad (analogous to <<=)
-- if one fails we want the whole evaluation to fail
getSquare (VectorBoard b) (r,c) = do
    row <- b V.!? r
    sq <- row V.!? c
    return sq

-- Test getSquare
-- get all squares in a flat list :: [Square]
-- [getSquare initialBoard (x,y) | x <- [0..7], y <- [0..7]] 
-- LHS: V.fromList [getSquare initialBoard (x,y) | x <- [0..7], y <- [0..7]] 
-- :: V.Vector Square
-- RHS: V.concat[initialBoard V.! r | r <- [0..7]]
-- test LHS and RHS for equality

-- replace x-th vector of the board with (the replacement of y-th element of the x-th vector with a newfigure)
setSquare :: VectorBoard -> Square -> Position -> VectorBoard
setSquare (VectorBoard b) newfig (x,y) = VectorBoard $ b V.// [(x, (b V.! x V.// [(y, newfig)]))]

-- putStr $ unlines [unwords [show (arr V.! x V.! y) | x <- [0..7], y <- [0..7]]]

-- make an element a king when it reaches the appropriate row
kingify :: Square -> Square
kingify (Empty) = Empty
kingify (Tile player _) = Tile player King

-- does the given tile belong to the given player
whoseTile :: Square -> Player -> Bool
whoseTile Empty _ = False
whoseTile (Tile player1 _) player2 = player1 == player2

-- check whether a given position is within the range of the board and EMPTY as well
canMoveInto :: VectorBoard -> Position -> Bool
canMoveInto (VectorBoard b) dest@(row, col) = row<8 && row>=0 && col<8 && col>=0 && getSquare (VectorBoard b) dest == Just Empty

-- test canMoveInto
-- [canMoveInto initialBoard x | x <- [(-1,-1), (1, -1), (7,7), (7,8), (8,7), (0,0), (5,2), (4,7), (2,5), (2,3), (2,4)]]

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
                                                    not $ z `elem` [Just (Tile White Man), Just (Tile White King), Just Empty] ]

        |Just (Tile Black Man) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) [x | (x,y) <- blackZippedPath, 
                                                    let z = getSquare (VectorBoard b) y, 
                                                    not $ z `elem` [Just (Tile Black Man), Just (Tile Black King), Just Empty] ]
                                                                                                        
        |Just (Tile White King) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) [x | (x,y) <- kingZippedPath, 
                                                    let z = getSquare (VectorBoard b) y, 
                                                    not $ z `elem` [Just (Tile White Man), Just (Tile White King), Just Empty] ]

        |Just (Tile Black King) <- getSquare (VectorBoard b) orig =
            filter (canMoveInto (VectorBoard b)) [x | (x,y) <- kingZippedPath, 
                                                    let z = getSquare (VectorBoard b) y, 
                                                    not $ z `elem` [Just (Tile Black Man), Just (Tile Black King), Just Empty] ]
        
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
