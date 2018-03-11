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


-- check whether a given position is within the range of the board and EMPTY as well
canMoveIntoDest :: VectorBoard -> Position -> Bool
canMoveIntoDest (VectorBoard b) dest = getSquare (VectorBoard b) dest == Just Empty &&  getSquare (VectorBoard b) dest /= Nothing


-- Get the number of figures each player has on the board, with the result in the form of (Black, White)
figureCount :: VectorBoard -> (Int,Int)
-- fold on current element tuple and the rest of the list, matching on it
figureCount board = foldl' (\(b,w) r -> case r of 
                                            Just (Tile Black _) -> (b+1,w)
                                            Just (Tile White _) -> (b,w+1)
                                            _ -> (b,w)
                            ) (0,0) sqs where
   sqs = filter (\n -> n /= Just Empty) [getSquare board (row, col) | row <- [0..7], col <- [0..7]]


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



-- ********************
-- ***** FOR TESTING PURPOSES- BORROWED FROM BOARDVECTORSPEC *****
-- ********************

-- generateRow :: [Int] -> Square -> V.Vector Square
-- generateRow ys sq = V.replicate 8 Empty V.// (\x -> [(z,sq) | z <- x]) ys

-- changeRow :: V.Vector Square -> [Int] -> Square -> V.Vector Square
-- changeRow vect ys sq = vect V.// (\x -> [(z,sq) | z <- x]) ys

-- fullRow :: [Int] -> Square -> [Int] -> Square -> V.Vector Square
-- fullRow xs s1 ys x2 = changeRow (generateRow xs s1) ys x2

-- badBoard :: VectorBoard
-- -- white's turn
-- badBoard = VectorBoard $ V.fromList [r0, r1, r2, r3, r4, r5, r6, r7] where
--     b = Tile Black Man
--     w = Tile White Man
--     --bk = Tile Black King
--     --wk = Tile White King
--     --empty = V.replicate 8 Empty
--     r0 = generateRow [1,3,5,7] b
--     r1 = generateRow [0,2,4,6] b
--     r2 = generateRow [1,3,7] b
--     r3 = generateRow [6] b
--     r4 = generateRow [5] w
--     r5 = generateRow [0,2,6] w
--     r6 = generateRow [1,3,5,7] w
--     r7 =  generateRow [0,2,4,6] w

--     --getJumps (GameState badBoard White)
--     -- [((4,5),((2,7),(3,6)))] is WRONG

-- badBoard2 :: VectorBoard
-- -- black's turn
-- badBoard2 = VectorBoard $ V.fromList [r0, r1, empty, r3, r4, r5, r6, r7] where
--     b = Tile Black Man
--     w = Tile White Man
--     empty = V.replicate 8 Empty

--     r0 = generateRow [1,5,7] b
--     r1 = generateRow [0,2,4,6] b
--     --r2 = empty
--     r3 = generateRow [0,2] b
--     r4 = generateRow [5] b
--     r5 = generateRow [0,4,6] w
--     r6 = fullRow [1,3] w [5] b
--     r7 =  generateRow [0,2,6] w

--     --getJumps (GameState badBoard2 Black)
--     --[((4,5),((6,7),(5,4)))] is wrong
--     -- inbetween is wrong!!! FIXED. Issue was filtering