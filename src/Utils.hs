module Utils where

import Data.List (intersperse, concat, foldl', sortBy, foldl1', genericLength, zip5, zip6)
import Data.Maybe (fromMaybe, fromJust, isJust, isNothing)
import qualified Data.Vector as V
import Control.Monad

import Data.Vector.Unboxed (create, freeze)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Map.Strict as M
-- import qualified Data.Vector.Unboxed.Mutable as MUV
import System.Random

import Control.Monad.Random
import System.Random.Mersenne.Pure64
import Control.Applicative
import Data.Function (on)

-- for date time generation
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Calendar

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

allPositionsInOrder :: [Position]
allPositionsInOrder = sortBy (compare `on` fst) (evenrows ++ oddrows)
        where
             -- even rows means odd columns 
             evenrows = liftA2 (,) [x| x <-[0..7], even x] [y | y <- [0..7], odd y]
             -- odd rows means even columns
             oddrows  = liftA2 (,) [x| x <-[0..7], odd x ] [y | y <- [0..7], even y]


listOfTuplesIndices :: [(Position, Int)]
listOfTuplesIndices = zip allPositionsInOrder [0..31]
           

positionIndicesMap = M.fromList listOfTuplesIndices  

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

-- ** STATISTICS HELPERS **

-- Variance
pvar :: (Floating a) => [a] -> a
pvar xs = sum sqDiffs / len
    where 
        len = genericLength xs
        mean = sum xs / len
        sqDiffs = map (\n -> (n-mean)^2) xs


-- Standard deviation
stdDev :: (Floating a) => [a] -> a
stdDev xs = sqrt $ pvar xs


-- Smallest element
smallestElem :: (Integral a) => [a] -> a
smallestElem xs = foldl1' min xs

-- Largest element
largestElem :: (Integral a) => [a] -> a 
largestElem xs = foldl1' max xs

-- ** STATISTICS (CALL THESE) WRAPPERS

-- Mean fitness of each generation
mean :: [[(Genome Double, Int)]] -> [Double]
mean xs = zipWith (/) sums lengths
    where
        -- get the sum of the fitnesses from the tuple
        sums = map fromIntegral (map (sum . map snd) xs)
        -- get the length of each sublist
        lengths = map genericLength xs


-- Standard Deviation and Population Variance as a tuple for each generation
sdAndvar :: [[(Genome Double, Int)]] -> [(Double, Double)]
sdAndvar xs = zipWith (,) sd variance 
    where
        sd = map stdDev fitnesses
        variance = map pvar fitnesses
        fitnesses = map (map fromIntegral) (map (map snd) xs)

-- Returns the smallest and largest elements from each generation       
minAndmaxElems :: [[(Genome Double, Int)]] -> [(Int, Int)]
minAndmaxElems xs = zipWith (,) minElem maxElem
    where
        minElem = map smallestElem fitnesses
        maxElem = map largestElem fitnesses
        fitnesses = map (map snd) xs


getEvolutionaryStats :: [Double] -> [(Double, Double)] -> [(Int,Int)] -> String
getEvolutionaryStats _mean _sdvar _minmax = unlines $ map show $ zip6 genNumber _mean sd v smallest largest
    where
    genNumber = [1..] :: [Int]
    sd = map fst _sdvar
    v = map snd _sdvar
    smallest = map fst _minmax
    largest = map snd _minmax

-- ********************************
-- *** RANDOM GENOME GENERATION ***
-- ********************************

randomGenomes :: (RandomGen g, Random a, Enum a) => Int -> Int -> a -> a -> Rand g [Genome a]
randomGenomes len genomeLen from to = do
    l <- replicateM (len*genomeLen) $ getRandomR (from,to)
    return $ nLists genomeLen l
    where 
        nLists :: Int -> [a] -> [[a]]
        nLists _ [] = []
        nLists n ls = take n ls : nLists n (drop n ls)


-- Randomly return a list of N opponents chosen from a given list of genomes
randomNopponents :: Int -> [Genome Double] -> Rand PureMT [Genome Double] 
randomNopponents n population = do
    let poplen = length population
    (map (population!!)) <$> (replicateM n $ getRandomR(0::Int,poplen))


-- ********************************        
-- ************** IO **************
-- ********************************

evalprint :: Show a => Rand PureMT [a] -> PureMT -> IO ()
evalprint list gen = mapM_ print (evalRand list gen)


getDateTime :: IO String
getDateTime = do
    now <- getCurrentTime
    let (year, month, day) = toGregorian $ utctDay now
    timezone <- getCurrentTimeZone
    let (TimeOfDay hour minute second) = localTimeOfDay $ utcToLocalTime timezone now
    return $ show day ++ "-" ++ show month ++ "-" ++ show hour ++ "h" ++ show minute

getFileName :: (IO String, IO String)
getFileName = (("EA-" ++) <$> (getDateTime) , ("EAStats-" ++) <$> (getDateTime))

getApopulationFromFile :: String -> IO [Genome Double]
getApopulationFromFile file = do
    contents <- readFile file
    let pop = (read contents)::[Genome Double]
    return pop
