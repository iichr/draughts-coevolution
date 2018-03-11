module EvolutionSpec where

import Test.Hspec
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, fromJust)
import System.IO
import Control.Monad.Random
import System.Random.Mersenne.Pure64
import Data.List
-- for comparing
import Data.Ord

import Evolution
import Utils

convertToGenome :: [[Double]] -> [Genome Double]
convertToGenome ds = ds

spec :: Spec
spec = do

    describe "randomGenomes" $ do
        it "should return a genome of zeros when called with the range of 0.0 to 0.0" $ do
            g <- newPureMT
            let hundredOppsZerosOnly    =   evalRand (randomGenomes 60 64 (0.0::Double) (0.0::Double) ) g
            hundredOppsZerosOnly `shouldBe` convertToGenome [replicate 64 x | x <- replicate 60 (0.0::Double)]
        it "should return the proper length of both each genome and the list of genomes" $ do
            g <- newPureMT
            let numberofGenomes = 60
            let genomeLength = 64
            let hundredOppsPlusToPlus   =   evalRand (randomGenomes numberofGenomes genomeLength (0.0::Double) (1.0::Double) ) g
            length hundredOppsPlusToPlus `shouldBe` 60
            length (filter (/=(genomeLength)) [length singleGenome | singleGenome <- hundredOppsPlusToPlus]) `shouldBe` 0
            length (filter (==(genomeLength)) [length singleGenome | singleGenome <- hundredOppsPlusToPlus]) `shouldBe` numberofGenomes
        it "all elements should be within the given range" $ do
            g <- newPureMT
            let numberofGenomes = 60
            let genomeLength = 64

            let hundredOppsPlusToPlus   =   evalRand (randomGenomes numberofGenomes genomeLength (0.0::Double) (1.0::Double) ) g
            length ([x | singleGenome <- hundredOppsPlusToPlus, x <- singleGenome, x<(0.0::Double), x > (1.0::Double)]) `shouldBe` 0

            let hundredOppsMinusToPlus  =   evalRand (randomGenomes numberofGenomes genomeLength (-1.0::Double) (1.0::Double) ) g
            length ([x | singleGenome <- hundredOppsMinusToPlus, x <- singleGenome, x<(-1.0::Double), x > (1.0::Double)]) `shouldBe` 0

    -- TODO implement the following tests from main as specs:
    -- -- ************************
    -- -- * TESTS
    -- let evalsingletest = evalRand (mapM (\opps -> evaluateNoCoin opps genOnesOnly) twentyFixedRandomGenomes) g
    -- let ones = length $ filter (==(1)) evalsingletest
    -- let minusones = length $ filter (==(-1)) evalsingletest
    -- let draws = length $ filter (==(0)) evalsingletest
    -- print (minusones,ones,draws)
    -- --print evalsingletest

    -- -- ************************
    -- -- * TEST as above but returns the number of -1s only (wins of player provided - White)
    -- let evalagainstmultiple = evalRand (evaluateNoCoinAgainstMultiple twentyFixedRandomGenomes genOnesOnly) g
    -- print $ evalagainstmultiple == minusones

    -- -- ************************
    -- -- * TEST EVALUATE TO TUPLE
    -- let gen12 = gen1:gen2:[]
    -- let evaluateToTuples = evaluateToTuple gen12 twentyFixedRandomGenomes
    -- let c = [(a,cc) | (a,b) <- evaluateToTuples, let cc = evalRand b g]
    -- print $ "Number of white wins per given genome: " ++ show c

    -- -- ***********************
    -- -- * VERIFY CORRECTNESS OF EVALUATION TO TUPLE - testing
    -- let evaltest2 = evalRand (mapM (\opps -> evaluateNoCoin opps gen1) twentyFixedRandomGenomes) g
    -- print $ (length $ filter (==(-1)) evaltest2) == (snd $ head c)
    -- let evaltest3 = evalRand (mapM (\opps -> evaluateNoCoin opps gen2) twentyFixedRandomGenomes) g
    -- print $ (length $ filter (==(-1)) evaltest3) == (snd $ head $ tail c)

    -- -- ************************
    -- -- * TESTS TO RUN FOR TWO SINGLE GENOMES, TEST IS REPEATED k TIMES
    -- -- let evalsingletest = evaluateNoCoin genOnesOnly gen1
    -- -- let l = sequence (replicate 50 evalsingletest)
    -- -- let p = evalRand l g
    -- -- let ones = length $ filter (==(1)) p
    -- -- let minusones = length $ filter (==(-1)) p
    -- -- let draws = length $ filter (==(0)) p
    -- -- print (minusones,ones,draws)



            