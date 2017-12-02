module Board where

-- not lazy (for now)
import qualified Data.Map as M

data Player = Black | White
    deriving (Eq, Show)

data Figure = Man | King
    deriving (Eq, Show)

data Square = Empty | Tile Player Figure
    deriving Eq

type Position = (Int, Int)
type PositionMap = M.Map Position Square

data Board = Board PositionMap
    deriving Eq

data GameState = GameState Board Player

instance Show Square where
    show Empty = "."
    show (Tile Black Man) = "b"
    show (Tile Black King) = "B"
    show (Tile White Man) = "w"
    show (Tile White King) = "W"

-- get all 64 board positions
-- TODO consider using as a lamba in genPositionMap
getPositions::[Position]
getPositions = [(row, col) | row <- [1..8], col <- [1..8]]

-- TEMP generate a blank position map with empty tiles. for testing purposes
-- generate position map with no figures given a list of positions
genPositionMap :: [Position] -> PositionMap
genPositionMap pss = M.fromList $ zip pss (repeat Empty)

-- TEMP generate a blank board with no figures. for testing purposes
-- TODO display board - need instance of show
getBoard :: Board
getBoard = Board $ genPositionMap getPositions

-- get the position map given a board
getBoardPositionMap :: Board -> PositionMap
getBoardPositionMap (Board posmap) = posmap

-- update board positions by providing a new positionmap
setBoardPositionMap :: Board -> PositionMap -> Board
setBoardPositionMap (Board _) newposmap = Board newposmap

-- generate black positions
-- they start at the top from (1,2) and then alternating - 12 total pieces
-- (ROW -  columns)
-- (1 - 2,4,6,8) = 1 and even
-- (2 - 1,3,5,7) = 2 and odd
-- (3 - 2,4,6,8) = 3 and even
blackInitialPositions :: PositionMap
blackInitialPositions = M.fromList $ [((row,col), Tile Black Man) | row <- [1..3], col <- [1..8], 
                                    odd row && even col || even row && odd col ]

-- generate white positions
-- startat bottom (6,1) and then alternate - 12 total pieces
-- (6 - 1,3,5,7)
-- (7 - 2,4,6,8)
-- (8 - 1,3,5,7)
whiteInitialPositions :: PositionMap
whiteInitialPositions = M.fromList $ [((row, col), Tile White Man) | row <- [6..8], col <- [1..8],
                                    odd row && even col || even row && odd col ]

startingBoard :: Board
startingBoard = undefined 

-- update the board when a new move is made. A move should consist of:
-- a position tuple, a figure to be moved (including Player)
-- insert syntax: insert k v map
-- Position and Square are a Key-Value pair respectively
updateBoard_onMove :: Board -> Position -> Square -> Board
updateBoard_onMove (Board posmap) newpos tile = setBoardPositionMap (Board posmap) (M.insert newpos tile posmap)




--better representation - binary? look into chess
-- more hypotheses perhaps, running simulation anyway



