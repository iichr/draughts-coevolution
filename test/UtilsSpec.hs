module UtilsSpec where

import Test.Hspec
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, fromJust)
import System.IO
import Control.Monad.Random
import System.Random.Mersenne.Pure64
import Data.List
-- for comparing
import Data.Ord

import Utils

listOfTriples :: [(Position,(Position, Position))]
listOfTriples = [((2,4),((4,2),(2,7))),((2,4),((4,2),(4,3))),((2,4),((4,2),(4,4))),((2,4),((4,2),(4,5))),((2,4),((4,2),(4,6))),((2,4),((4,2),(4,7))),((2,4),((4,4),(2,3))),((2,4),((4,4),(2,4))),((2,4),((4,4),(2,5))),((2,4),((4,4),(2,6))),((2,4),((4,4),(2,7))),((2,4),((4,4),(4,3))),((2,4),((4,4),(4,4))),((2,4),((4,4),(4,5))),((2,4),((4,4),(4,6))),((2,4),((4,4),(4,7))),((2,4),((4,6),(2,3))),((2,4),((4,6),(2,4))),((2,4),((4,6),(2,5))),((2,4),((4,6),(2,6))),((2,4),((4,6),(2,7))),((2,4),((4,6),(4,3))),((2,4),((4,6),(4,4))),((2,4),((4,6),(4,5))),((2,4),((4,6),(4,6))),((2,4),((4,6),(4,7))),((2,4),((5,2),(2,3))),((2,4),((5,2),(2,4))),((2,4),((5,2),(2,5))),((2,4),((5,2),(2,6))),((2,4),((5,2),(2,7))),((2,4),((5,2),(4,3))),((2,4),((5,2),(4,4))),((2,4),((5,2),(4,5))),((2,4),((5,2),(4,6))),((2,4),((5,2),(4,7))),((2,4),((5,4),(2,3))),((2,4),((5,4),(2,4))),((2,4),((5,4),(2,5))),((2,4),((5,4),(2,6))),((2,4),((5,4),(2,7))),((2,4),((5,4),(4,3))),((2,4),((5,4),(4,4))),((2,4),((5,4),(4,5))),((2,4),((5,4),(4,6))),((2,4),((5,4),(4,7))),((2,4),((5,6),(2,3))),((2,4),((5,6),(2,4))),((2,4),((5,6),(2,5))),((2,4),((5,6),(2,6))),((2,4),((5,6),(2,7))),((2,4),((5,6),(4,3))),((2,4),((5,6),(4,4))),((2,4),((5,6),(4,5))),((2,4),((5,6),(4,6))),((2,4),((5,6),(4,7))),((2,4),((6,2),(2,3))),((2,4),((6,2),(2,4))),((2,4),((6,2),(2,5))),((2,4),((6,2),(2,6))),((2,4),((6,2),(2,7))),((2,4),((6,2),(4,3))),((2,4),((6,2),(4,4))),((2,4),((6,2),(4,5))),((2,4),((6,2),(4,6))),((2,4),((6,2),(4,7))),((2,4),((6,4),(2,3))),((2,4),((6,4),(2,4))),((2,4),((6,4),(2,5))),((2,4),((6,4),(2,6))),((2,4),((6,4),(2,7))),((2,4),((6,4),(4,3))),((2,4),((6,4),(4,4))),((2,4),((6,4),(4,5))),((2,4),((6,4),(4,6))),((2,4),((6,4),(4,7))),((2,4),((6,6),(2,3))),((2,4),((6,6),(2,4))),((2,4),((6,6),(2,5))),((2,4),((6,6),(2,6)))]

spec :: Spec
spec = do

    describe "listOfTuplesIndices" $ do
        it "should return a list of position, int tuples of length 32" $ do
            length listOfTuplesIndices `shouldBe` 32
        it "should contain only the positions which are playable (reachable)" $ do
            listOfTuplesIndices `shouldBe` [((0,1),0),((0,3),1),((0,5),2),((0,7),3),((1,0),4),((1,2),5),((1,4),6),((1,6),7),((2,1),8),((2,3),9),((2,5),10),((2,7),11),((3,0),12),((3,2),13),((3,4),14),((3,6),15),((4,1),16),((4,3),17),((4,5),18),((4,7),19),((5,0),20),((5,2),21),((5,4),22),((5,6),23),((6,1),24),((6,3),25),((6,5),26),((6,7),27),((7,0),28),((7,2),29),((7,4),30),((7,6),31)]

    describe "flattenJump" $ do
        it "should convert (origin, (dest, inbetw)) to (origin, inbetw, dest)" $ do
            flattenJump ((0,1),((0,3), (0,2))) `shouldBe` ((0,1), (0,2), (0,3))
            flattenJump ((6,4),((3,7), (1,8))) `shouldBe` ((6,4), (1,8), (3,7))

    describe "inbetweenPosition" $ do
        it "given an origin and destination position and a list of triples returns the correct inbetween position" $ do
            getInbetweenPosition ((1,3),(2,3)) [((2,3),((4,5),(6,7))), ((7,8),((2,3),(4,9))), ((1,3),((2,3),(0,0)))]
            `shouldBe` Just (0,0)
        it "returns Nothing if list of positions to search through is empty" $ do
            getInbetweenPosition ((1,3),(2,3)) [((2,3),((4,5),(6,7))), ((7,8),((2,3),(4,9))), ((1,3),((3,3),(0,0)))]
            `shouldBe` Nothing

    describe "convertPos2Index" $ do
        it "given a board position converts it to an index in a 1D array" $ do
            map convertPos2Index [(0,0), (0,5), (0,7), (2,4), (2,5), (3,3), (3,7), (4,0), (5,1), (6,3), (6,6), (6,7), (7,0), (7,2), (7,7)]
            `shouldBe`
            [0, 5, 7, 20, 21, 27, 31, 32, 41, 51, 54, 55, 56, 58, 63] 
           
    describe "getEvolutionaryStats" $ do
        it "should return a comma separated list line by line, each list with 4 components" $ do
            let string = getEvolutionaryStats [0.1, 0.24, 0.213983] [(0.23,0.64),(2.342,29.45),(23.52,90.42113)] [(0,8),(1,14),(3,12)]
            string `shouldBe` "1,0.1,0.23,0,8\n2,0.24,2.342,1,14\n3,0.213983,23.52,3,12\n"
    describe "getApopulationFromFile" $ do
        it "should parse a population from a file and return it as a list of genomes of the correct length" $ do
            let popSize = 4500
            testpop <- getApopulationFromFile "bestrandom07.txt"
            length testpop `shouldBe` popSize
            testpop05 <- getApopulationFromFile "bestrandom05.txt"
            length testpop05 `shouldBe` popSize