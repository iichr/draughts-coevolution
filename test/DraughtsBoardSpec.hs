module DraughtsBoardSpec where

import Test.Hspec
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, fromJust)

import Utils
import DraughtsBoard
import GamePlay


getBoardFromState :: GameState -> VectorBoard
getBoardFromState (GameState (VectorBoard b) _) = VectorBoard b
-- also useful (\(VectorBoard x)->x) board

whiteLostBoard :: VectorBoard
whiteLostBoard = VectorBoard $ V.fromList [r0, empty, empty, r3, empty, empty, r6, empty] where
    empty = V.replicate 8 Empty
    bk = Tile Black King
    b = Tile Black Man
    r0 = generateRow [1,5] bk
    r3 = generateRow [0,2,4] b
    r6 = generateRow [3,5,7] bk


testBoard2 :: VectorBoard
testBoard2 = VectorBoard $ V.fromList [r0, r1, r2, r3, r4, r5, r6, r7] where
    b = Tile Black Man
    w = Tile White Man
    --bk = Tile Black King
    --wk = Tile White King
    --empty = V.replicate 8 Empty
    r0 = generateRow [7] b
    r1 = generateRow [2,4,6] b
    r2 = generateRow [3,5,7] b
    r3 = generateRow [0,2] b
    r4 = fullRow [1] b [3,5,7] w
    r5 = generateRow [0,2,4,6] w
    r6 = generateRow [3,7] w
    r7 =  generateRow [0] w

testBoard3 :: VectorBoard
testBoard3 = VectorBoard $ V.fromList [r0, r1, r2, r3, r4, r5, r6, empty] where
    b = Tile Black Man
    w = Tile White Man
    wk = Tile White King
    empty = V.replicate 8 Empty
    r0 = generateRow [1] wk
    r1 = generateRow [4] wk
    r2 = generateRow [3] w
    r3 = generateRow [0,2,6] b
    r4 = fullRow [1,3] b [7] w
    r5 = generateRow [0,6] w
    r6 = generateRow [1,3,7] w

testBoard4 :: VectorBoard
testBoard4 = VectorBoard $ V.fromList [r0, r1, r2, empty, r4, r5, r6, r7]  where
    b = Tile Black Man
    w = Tile White Man
    bk = Tile Black King
    empty = V.replicate 8 Empty
    r0 = generateRow [7] b
    r1 = generateRow [0,4,6] b
    r2 = generateRow [1,3,5] b
    r4 = fullRow [1,5] w [7] b
    r5 = generateRow [0] w
    r6 = fullRow [1,5] w [7] b
    r7 = fullRow [0] w [2] bk


testBoard1 :: VectorBoard
testBoard1 = VectorBoard $ V.fromList [r0, r1, r2, empty, r4, empty, r6, empty] where  
        b = Tile Black Man
        w = Tile White Man
        bk = Tile Black King
        empty = V.replicate 8 Empty
        r0 = generateRow [1,3,5,7] b
        r1 = generateRow [4] b
        r2 = fullRow [1] w [7] b
        r4 = fullRow [7] w [5] bk
        r6 = fullRow [3] b [7] w

-- Endspiel board
testBoard5 :: VectorBoard
testBoard5 = VectorBoard $ V.fromList [r0, r1, r2, empty, empty, r5, r6, r7] where
    bk = Tile Black King
    wk = Tile White King
    empty = V.replicate 8 Empty
    r0 = generateRow [1,3,5,7] wk
    r1 = generateRow [2] wk
    r2 = generateRow [3,5] wk
    r5 = generateRow [2] bk
    r6 = generateRow [3] bk
    r7 = generateRow [2] bk


generateRow :: [Int] -> Square -> V.Vector Square
generateRow ys sq = V.replicate 8 Empty V.// (\x -> [(z,sq) | z <- x]) ys

changeRow :: V.Vector Square -> [Int] -> Square -> V.Vector Square
changeRow vect ys sq = vect V.// (\x -> [(z,sq) | z <- x]) ys

fullRow :: [Int] -> Square -> [Int] -> Square -> V.Vector Square
fullRow xs s1 ys x2 = changeRow (generateRow xs s1) ys x2    

spec :: Spec
spec = do
    describe "getSquare" $ do
        it "should return all sqaures on the board in a flat list" $ do
            let boardgetSq = V.fromList [ getSquare initialBoard (x,y) | x <- [0..7], y <- [0..7]] 
            let rhs = V.concat [(\ (VectorBoard x) -> x) initialBoard V.! r | r <- [0..7]]
            boardgetSq `shouldBe` rhs

    describe "shouldPromote" $ do
        it "should indicate whether a piece is promoted at the end of the board" $ do
            [shouldPromote Black (x,7) | x <- [0..7]] `shouldBe` replicate 7 False ++ [True]
        it "should promote all Black pieces at the 7th row" $ do
            [shouldPromote Black (7,x) | x <- [0..7]] `shouldBe` replicate 8 True
        it "should not promote any pieces given a non-existent row number" $ do
            [shouldPromote White (20,x) | x <- [0..12]] `shouldBe` replicate 13 False
        it "should promote all White pieces at the 0th row" $ do
            [shouldPromote White (0,x) | x <- [0..7]] `shouldBe` replicate 8 True
        it "should not promote White pieces at the 7th row" $ do
            [shouldPromote White (7,x) | x <- [0..7]] `shouldBe` replicate 8 False
        it "should not promote Black pieces at the 0th row" $ do
            [shouldPromote Black (0,x) | x <- [0..7]] `shouldBe` replicate 8 False
    
    describe "kingify" $ do
        it "turns an ordinary Man tile into a King tile" $ do
            map kingify [Tile Black Man, Tile White Man] `shouldBe` [Tile Black King, Tile White King]
        it "does not promote empty tiles" $ do
            kingify Empty `shouldBe` Empty

    describe "whoseTile" $ do
        context "when given a tile and a player" $ do
            it "should return true if the tile belongs to the player" $ do
                whoseTile (Tile White Man) White `shouldBe` True
                whoseTile (Tile White King) White `shouldBe` True
                whoseTile (Tile Black King) Black `shouldBe` True
                whoseTile (Tile Black Man) Black `shouldBe` True
            it "should return false if the tile does not belong to the player" $ do
                whoseTile (Tile White Man) Black `shouldBe` False
                whoseTile (Tile Black King) White `shouldBe` False
            it "should return false if tile is empty or nothing" $ do
                whoseTile (Empty) Black `shouldBe` False
                whoseTile (Empty) White `shouldBe` False
    
    describe "canMoveIntoDest" $ do
        it "is false when on or both of the elements of position is/are out of the board" $ do
            [canMoveIntoDest initialBoard (x,y) | x <- [-1,-2,-3,-15,28734,8,9,13], y <- [1,2,3,4,0,-121,-31,424]]
            `shouldBe` replicate 64 False
        it "is false when the position to move into is not empty" $ do
            let alloccupied = [(y,x)| x<-[0,2,4,6],y <- [1,5,7]] ++ [(z,k) | z <- [0,2,6], k<-[1,3,5,7]]
            [canMoveIntoDest initialBoard pss | pss <- alloccupied] `shouldBe` replicate 24 False
        it "is true when the position is within the board and empty" $ do
            let allunoccupied = [(x,y)| x<-[0,2,6],y <- [0,2,4,6]] ++ [(z,k) | z <- [1,5,7], k<-[1,3,5,7]] ++ [(3,u)|u<-[0..7]] ++ [(4,v)|v<-[0..7]]
            [canMoveIntoDest initialBoard pss | pss <- allunoccupied] `shouldBe` replicate 40 True

    describe "simpleMove" $ do
        context "when given a board and a position that is empty" $ do
            it "returns an empty list of possible simple moves" $ do
                let validpos = [(0,0), (1,1), (2,4), (5,3), (5,7), (4,2), (3,6), (7,3)]
                -- for safe evaluation, now redundant
                -- let invalid = [(u,v) | u <- [-1,-2,-3,-15,28734,8,9,13], v <- [1,2,4,8,0,-121,-31,424]]
                concat [simpleMove initialBoard x | x <- validpos] `shouldBe` []
                -- for safe evaluation, now redundant
                --concat [simpleMove initialBoard y | y <- invalid] `shouldBe` []
        context "when given a board and a valid position with no obstacles in the direction of movement" $ do
            it "returns a list of possible simple moves" $ do
                simpleMove initialBoard (2,1) `shouldBe` [(3,0),(3,2)]
                simpleMove initialBoard (2,5) `shouldBe` [(3,4), (3,6)]
                simpleMove initialBoard (2,7) `shouldBe` [(3,6)]
                simpleMove initialBoard (5,0) `shouldBe` [(4,1)]
                simpleMove initialBoard (5,6) `shouldBe` [(4,5),(4,7)]
        context "when given a board and a valid position from which there are no possible moves" $ do
            it "returns an empty list" $ do
                let allOccupiedBlocked = [(y,x)| x<-[0,2,4,6],y <- [1,7]] ++ [(z,k) | z <- [0,6], k<-[1,3,5,7]]
                concat [simpleMove initialBoard z | z <- allOccupiedBlocked] `shouldSatisfy` null

    describe "figureCount" $ do
        it "given an empty board should not crash but return 0 figures each" $ do
            figureCount (VectorBoard $ V.replicate 8 (V.replicate 8 Empty)) `shouldBe` (0,0)
        it "given a non-empty board should return the number of Black and White figures in a tuple in that order" $ do
            figureCount initialBoard `shouldBe` (12,12)
            figureCount testBoard5 `shouldBe` (3,7)
            figureCount testBoard1 `shouldBe` (8,3)
            figureCount testBoard2 `shouldBe` (10,10)
            figureCount testBoard4 `shouldBe` (10,6)

    describe "whoWon" $ do
        it "should return that a player has won if there are no figures belonging to the opponent on the board" $ do
            whoWon (GameState whiteLostBoard White) `shouldBe` Just Black
            whoWon (GameState whiteLostBoard Black) `shouldBe` Just Black
        it "should return Nothing if both players still have figures left on the board" $ do
            whoWon (GameState testBoard1 Black) `shouldBe` Nothing
            whoWon (GameState testBoard2 White) `shouldBe` Nothing
            whoWon (GameState testBoard3 Black) `shouldBe` Nothing
            whoWon (GameState testBoard4 White) `shouldBe` Nothing
            whoWon (GameState testBoard5 Black) `shouldBe` Nothing