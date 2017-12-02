module Boardvector where

import Data.List (intersperse, concat)
import qualified Data.Vector as V
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
getSquare :: VectorBoard -> Position -> Square
getSquare (VectorBoard b) (r,c) = b V.! r V.! c

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


        