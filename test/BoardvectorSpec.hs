module BoardvectorSpec where

import Test.Hspec
import Boardvector
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, fromJust)

spec :: Spec
spec = do
    describe "getSquare" $ do
        it "should return all sqaures on the board in a flat list" $ do
            let boardgetSq = V.fromList [fromJust $ getSquare initialBoard (x,y) | x <- [0..7], y <- [0..7]] 
            let rhs = V.concat $ map fromJust [(\ (VectorBoard x) -> x) initialBoard V.!? r | r <- [0..7]]
            boardgetSq `shouldBe` rhs

-- Test getSquare
-- get all squares in a flat list :: [Square]
-- [getSquare initialBoard (x,y) | x <- [0..7], y <- [0..7]] 
-- LHS: V.fromList [getSquare initialBoard (x,y) | x <- [0..7], y <- [0..7]] 
-- :: V.Vector Square
-- RHS: V.concat[initialBoard V.! r | r <- [0..7]]
-- test LHS and RHS for equality

-- extract V.Vector Square (V.Vector Square) from a VectorBoard 
-- (\ (VectorBoard x) -> x) initialBoard

-- putStr $ unlines [unwords [show (arr V.! x V.! y) | x <- [0..7], y <- [0..7]]]

-- Test shouldPromote
-- [shouldPromote Black (x,7) | x <- [0..7]]
-- [shouldPromote Black (7,x) | x <- [0..7]]
-- [shouldPromote Black (20,x) | x <- [0..7]]
-- [shouldPromote White (8,x) | x <- [0..7]]
-- [shouldPromote White (0,x) | x <- [0..7]]
-- [shouldPromote White (7,x) | x <- [0..7]]

-- Test canMoveInto
-- [canMoveInto initialBoard x | x <- [(-1,-1), (1, -1), (7,7), (7,8), (8,7), (0,0), (5,2), (4,7), (2,5), (2,3), (2,4)]]

-- Test getPlayerMoves
-- getPlayerMoves (GameState initialBoard White)

-- Test performMove
-- let bb = setSquare initialBoard (Tile Black Man) (5,2)
-- let gs1 = performMove (GameState bb White) (6,1) (4,3)
-- let invalidgs2 = performMove gs1 (5,0) (4,1)
-- invalidgs2 == gs1
-- let validgs2 = performMove gs1 (2,3) (3,4)
-- validgs2 /= gs1
-- let validgs3 = performMove validgs2 (5,6) (4,5)
-- validgs3 /= validgs2
-- let validgs4 = performMove validgs3 (3,4) (5,6)
-- let validgs5 = performMove validgs4 (7,0) (6,1)
-- let blackKinged = performMove validgs5 (5,2) (7,0)
-- let gs6 = performMove blackKinged (4,3) (3,4)
-- let gs7 = performMove gs6 (5,0) (4,1)
-- gs6 == gs7
-- let blackKingJumpBack = performMove gs7 (7,0) (5,2)