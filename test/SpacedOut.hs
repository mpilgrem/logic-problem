-- Spaced Out (Logic Problems, Issue No. 388 2017, page 12)
-- Status: Solved

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
    (cs, ms) = solveLogicProblem rules [rr1]

-- Describe elements
instance Show A where
    show = showFrom [ "JXX7"
                    , "JXX12"
                    , "JXX14"
                    , "JXX19"
                    , "JXX26" ]
instance Show B where
    show = showFrom [ "Buzzard"
                    , "Condor"
                    , "Osprey"
                    , "Stormhawk"
                    , "Uchtik" ]
instance Show C where
    show = showFrom [ "CT-1"
                    , "CT-2"
                    , "CT-4"
                    , "Lunar City"
                    , "Woomera" ]
instance Show D where
    show = showFrom [ "Communications"
                    , "Fuel storage"
                    , "Gossec drive"
                    , "Life support"
                    , "Sensors" ]

-- Absolute rules
rules :: Rule
rules = r1 . r2 . r3 . r4 . r5 . r6 . r7
  where
    r1 = (C4 >|< B3) . (B3 <-> D5)
    r2 = (D2 <-> C1) . (B4 >|< D1)
    r3 = (A1 >|< C4) . (A1 >|< C5)
    r4 = (B1 <-> C5) . (B1 >|< A5)
    r5 = A4 <-> D4
    r6 = (A3 <-> C3) . (A3 >|< D3)
    r7 = A2 <-> B5

-- Relative rules
rr1 :: RRule
rr1 c1 c2
    | (a1,  _, C4,  _) <- c1
    , (a2, B3,  _, D5) <- c2
    , a2 < A5
    , a1 == succ a2
      = True
    | otherwise
      = False
