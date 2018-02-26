module Utils where

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

-- ** GENERAL GAME DATA TYPES **

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


-- ** EVOLUTIONARY DATA TYPES **

type Genome a = [a]

-- type FitnessFun a = Genome a -> Int

type SelectionFun a = [(Genome a, Double)] -> Rand PureMT [Genome a]

type Crossover a = (Genome a, Genome a) -> Rand PureMT (Genome a, Genome a)

type Mutation a = Genome a -> Rand PureMT (Genome a)



-- ** BOARD INSTANCES

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

instance Show GameState where
    show (GameState board player) = 
        show board ++ show player



-- ** BOARD FUNCTIONS **

-- convert a position in a 2D array in the row, column format to a 1D index; zero-indexed
convertPos2Index :: Position -> Int
convertPos2Index (row, col) = row * 8 + col



-- ** MOVE AUXILIARY FUNCTIONS **


-- Convert a jump triple in the form of (origin, (destination, inbetween)) 
-- to (origin, inbetween, destination)
flattenJump :: (Position, (Position, Position)) -> (Position, Position, Position)
flattenJump (a,(b,c)) = (a,c,b)

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


-- ** GENERAL GAME PLAYING **

-- Get the opposite player of the one given
oppositeOf :: Player -> Player
oppositeOf White = Black
oppositeOf Black = White