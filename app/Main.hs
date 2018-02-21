module Main where

import Lib
import Boardvector
import MoveSelectMechanical
import MoveSelectTree
import System.IO
import Evolution

import Control.Monad.Random
import System.Random.Mersenne.Pure64
import Data.List
-- for comparing
import Data.Ord

coin :: RandomGen g => Rand g Int
coin = getRandomR (0,1)

-- EVALUATION (fitness)
-- given a population of genomes P
-- generate 100 fixed oponents, keep them
-- play EACH individual from p
-- count how many times each individual won
-- zip that with the genome in the form of (genome, number of wins)
-- at the end map to obtain a list of [(genome, number of wins)]
evaluate :: Genome Double -> Genome Double -> Rand PureMT Int
evaluate genome op = do
    toss <- coin
    let ai = performMoveAIalphabeta4PlyNonIO
    let gs = (GameState initialBoard Black)
    let singleGenomeScores = if (toss == 0)
        then
            playnonIO 150 op genome ai ai gs
        else
            playnonIO 150 genome op ai ai gs 
    --let ones = length (filter (==(1)) singleGenomeScores)
    --let minusones = length (filter (==(-1)) singleGenomeScores)
    --let draws = length (filter (==(0)) singleGenomeScores)
    --return $ (genome, singleGenomeScores)
    return singleGenomeScores         

    -- where
    --     singleGenomeScores = [score | o <- opponents, let score = playnonIO 150 o genome ai ai gs]
    --     ai = performMoveAIalphabeta4PlyNonIO
    --     gs = (GameState initialBoard Black)
        
        -- ones = length (filter (==(1)) singleGenomeScores)
        -- minusones = length (filter (==(-1)) singleGenomeScores)
        -- draws = length (filter (==(0)) singleGenomeScores)


main :: IO ()
-- output is written immediately (never stored in the buffer)
main = do
    g <- newPureMT
    -- let p = evalRand (randomGenomes 2 64 (0.1::Double) (1.0::Double) ) g
    -- print p
    
    
    -- let gen2 = head p

    --head $ tail p
    --take 64 $ repeat (1.0::Double)
    hSetBuffering stdout NoBuffering
    -- using random moves
    -- play aiNextState aiNextState (GameState initialBoard White)
    
    
    let gen1 = [0.4623663455760598,0.35431921832038693,0.6465467201290843,0.20049506235587605,0.5486368738706038,0.3484576933501492,0.7799488340555901,0.6531892850324704,0.7910675507165263,0.8249837863890737,0.5018809073065953,0.5810061029739773,0.6199278428932582,0.7545650710965309,0.5825819528189086,0.6543336582015509,0.8860755424213383,0.5453502691103678,0.9001311555608781,0.58246937419012,0.6469219354129381,0.3344678302964224,0.7623646981288812,0.34383013632176346,0.21646169859809666,0.3865204633455521,0.7025229229966063,0.596679438393494,0.725206098133454,0.3438917451523287,0.4914239163879026,0.40325774836360373,0.25482848734141667,0.9806921433193129,0.6792896898590851,0.26596929481131665,0.7069147105525373,0.7992710408274454,0.36894294089615964,0.16856317767003792,0.5856359671379119,0.617365492113439,0.21822020262827457,0.46202692967917935,0.9557769756243938,0.2576337682107726,0.6299753356230534,0.5118679617006049,0.46863109179810747,0.4605902579271559,0.13291967886878142,0.7645245665911573,0.8804681605739184,0.7468861526352808,0.4205438522825835,0.27598733132583464,0.2918897637662213,0.12705114927368227,0.14256854831744306,0.9442884343959254,0.2554397646907857,0.9765103747450653,0.337053229673977,0.792711932791264]
    let gen2 = [0.6066810511090085,0.5768251704389419,0.6395518806556468,0.2499362023291403,0.757587312820649,0.957420988463747,0.871484402623842,0.34622885639728096,0.8937333419758359,0.8825691401903594,0.9989391812203718,0.3789127200832336,0.4552907286493282,0.5014536126987393,0.23863698998782748,0.9297506330156929,0.6360819122627712,0.776552718930624,0.4131778308968179,0.36547427397081245,0.41871632023670213,0.9181263721045636,0.9332697212689643,0.445029114582975,0.1495666082614455,0.6402003807735356,0.5117879575720573,0.23459972684157654,0.4773304239684546,0.6878464393012723,0.32224131760528263,0.39559838550086746,0.5093677493407782,0.5711765753817226,0.8674362034132423,0.8609714971172648,0.5900504837640177,0.53434162692029,0.17541495656220302,0.7418026102348098,0.2723653559647466,0.13239110596341172,0.2808239612180402,0.20093634380114064,0.6240155993490109,0.11252642702231563,0.42729091858728274,0.8915754543122129,0.2113337254741719,0.45906975270750183,0.6457222355873761,0.28186471812637476,0.45468069962913404,0.7658360136655404,0.7490193088093026,0.6339343926001751,0.5679424729611348,0.9192660410367524,0.7130027423568474,0.4848322015856794,0.8328345575882296,0.5849878497298652,0.8327505940026302,0.24908310908709502]
    let genOnesOnly = take 64 $ repeat (0.1::Double)
    -- let run = playnonIO 150 genOnesOnly gen1 performMoveAIalphabeta3PlyNonIO performMoveAIalphabeta3PlyNonIO (GameState initialBoard Black)
    -- play 150 genOnesOnly gen1 performMoveMinimax3Ply performMoveMinimax3Ply (GameState initialBoard Black)

    -- TEST RESULTS
    -- gen1 vs genOnesOnly = 1 Black win
    -- genOnesOnly vs gen1 = 1 Black win
    
    let hundredOpponents = evalRand (randomGenomes 60 64 (0.1::Double) (1.0::Double) ) g
    -- let evalsingletest = evalRand (mapM (\opps -> evaluate gen2 opps) hundredOpponents) g
    
    -- let evalsingletest = evalRand (mapM (\opps -> evaluate gen2 opps) hundredOpponents) g
    -- let ones = length $ filter (==(1)) evalsingletest
    -- let minusones = length $ filter (==(-1)) evalsingletest
    -- let draws = length $ filter (==(0)) evalsingletest
    -- print (minusones,ones,draws)

    -- normal alpha beta pruning without randomness
    let testalphabetadepthlim = evalRand (alphabetadepthlim' negInf posInf 0 6 (GameState initialBoard Black) genOnesOnly getSum) g
    --let mintest = minimum testalphabetadepthlim
    print testalphabetadepthlim
    -- let maxtest = maximumBy (comparing fst) testalphabetadepthlim
    -- print maxtest
    --print mintest
    --l <- replicateM (length $ testalphabetadepthlim) $ getRandomR ((-0.25)::Double,(0.25))
    --print l
    --let res = zipWith (+) testalphabetadepthlim l
    --print res
    -- for gen1 and depth 5: [4.992106123450303,6.06033551678479,6.564008542960001,5.220076695574086,5.220076695574086,6.604452783527415,6.444142430460917]
    -- for gen2 and depth 5: [4.798020665429608,4.908711777246716,5.832914146617489,5.687290515940105,6.055601871664655,6.329652916658873,6.2553778500666635]
    -- for onesonly and d 5: [5.73,5.9,5.9,5.73,5.73,6.3,6.3]
    -- print run
    -- ran 10 times with gen2 set to ones - 6 B 1 W 5 Draw