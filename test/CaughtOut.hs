-- Caught Out
-- Status: Solves puzzle

module Main where

import Control.Monad (unless)
import Game.LogicProblem ((<->), (>|<), A(..), B (..), C (..), D (..), Rule,
    RRule, showFrom, solveLogicProblem)

main :: IO ()
main = do
    putStrLn "Solved"
    putStrLn "------"
    mapM_ (\s -> putStrLn $ show s ++ "\n") cs
    unless (null ms) $ do
        putStrLn "Unresolved"
        putStrLn "----------"
        mapM_ (\s -> putStrLn $ show s ++ "\n") ms
  where
    (cs, ms) = solveLogicProblem rules []

instance Show A where
    show = showFrom [ "Chuck Ingett-Downe"
                    , "Kat St Dogges"
                    , "Klaud Burst"
                    , "Peter Patter"
                    , "Praesepe Tayshan" ]
instance Show B where
    show = showFrom [ "Airport"
                    , "Charity Shop"
                    , "Pub"
                    , "School Event"
                    , "Supermarket" ]
instance Show C where
    show = showFrom [ "Bus Shelter"
                    , "Holly bush"
                    , "Lych-gate"
                    , "Scaffolding"
                    , "Trolley-park" ]
instance Show D where
    show = showFrom [ "Case of wine"
                    , "Cello"
                    , "Dog-food"
                    , "Planter"
                    , "Suitcase" ]

-- Specific rules
r1 = B5 <-> D3
r2 = B1 <-> C2
r3 = (A2 >|< D5) . (C3 >|< D5) . (A2 >|< C3) . (A5 >|< C3)
r4 = (D4 <-> C4) . (A3 >|< D4) . (A3 >|< C4) . (A3 <-> B2) . (A3 >|< D1)
r5 = (A1 >|< B4) . (A4 <-> C5) . (A1 >|< B3) . (A4 >|< B3)
r6 = (A5 <-> D2) . (A5 >|< C1)
rules = r1 . r2 . r3 . r4 . r5 . r6
