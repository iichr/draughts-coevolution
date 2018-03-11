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
        it "should return a genome" $ do
            g <- newPureMT
            let hundredOppsZerosOnly    =   evalRand (randomGenomes 60 64 (0.0::Double) (0.0::Double) ) g
            hundredOppsZerosOnly `shouldBe` convertToGenome [concat $ replicate 60 x | let x = replicate 64 (0.0::Double)]