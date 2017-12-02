module Boardvector where

import qualified Data.Vector as V
import Board

newtype VectorBoard = VectorBoard (V.Vector (V.Vector Square))

getTile :: VectorBoard -> Position -> Square
getTile (VectorBoard b) (r,c) = b V.! r V.! c

blackrow = V.replicate 8 (Tile Black Man)
whiterow = V.replicate 8 (Tile White Man)
emptyrow = V.replicate 8 (Empty)

-- 1 and 3, 7(white)
initialOddRow :: V.Vector Square -> V.Vector Square
initialOddRow vect = vect V.// [(i, Empty) | i <- [0..7], even i]

-- 2, 6 and 8(white)
-- replace the vector element at position i by Empty
initialEvenRow :: V.Vector Square -> V.Vector Square
initialEvenRow vect = vect V.// [(i, Empty) | i <- [0..7], odd i]

-- TODO convert to VectorBoard type when Show instance is implemented
initialBoard :: V.Vector (V.Vector Square)
-- each elem in the concat is of type V.Vector (V.Vector Square)
initialBoard = V.concat [V.replicate 1 (initialOddRow blackrow),
                        V.replicate 1 (initialEvenRow blackrow),
                        V.replicate 1 (initialOddRow blackrow),
                        V.replicate 2 emptyrow, 
                        V.replicate 1 (initialEvenRow whiterow),
                        V.replicate 1 (initialOddRow whiterow),
                        V.replicate 1 (initialEvenRow whiterow)]

