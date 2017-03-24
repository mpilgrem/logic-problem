-- Upgrades (Logic Problems, Issue No. 388 2017, page 10)
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
    (cs, ms) = solveLogicProblem rules [rr3a, rr3b]

-- Describe elements
instance Show A where
    show = showFrom [ "Baz"
                    , "Caz"
                    , "Daz"
                    , "Jez"
                    , "Kez" ]
instance Show B where
    show = showFrom [ "£5"
                    , "£6"
                    , "£7"
                    , "£8"
                    , "£9" ]
instance Show C where
    show = showFrom [ "Dork"
                    , "Geek"
                    , "Genius"
                    , "Nerd"
                    , "Techie" ]
instance Show D where
    show = showFrom [ "Boots"
                    , "Comb"
                    , "Heels"
                    , "Jeans"
                    , "Tie" ]

-- Absolute rules
rules :: Rule
rules = r1 . r2 . r3 . r4 . r5
  where
    r1 = (A1 >|< B4) . (A1 >|< B5) . (A3 >|< B4) . (A3 >|< B5) . (A5 >|< B4) .
        (A5 >|< B5) . (A2 >|< B1) . (A2 >|< B2) . (A2 >|< B3) . (A4 >|< B1) .
        (A4 >|< B2) . (A4 >|< B3)
    r2 = C1 <-> D1
    r3 = (A5 <-> C4) . (A5 >|< D5) . (A2 >|< D5) . (A4 >|< D5)
    r4 = (B5 <-> D4) . (A1 <-> B3) . (A1 >|< D2)
    r5 = (A2 <-> D3) . (C3 >|< D1) . (C3 >|< D3) . (C3 >|< D4)

-- Relative rules
rr3a :: RRule
rr3a c1 c2
    | (A5, b1,  _,  _) <- c1
    , (A1, b2,  _, D5) <- c2
    , b1 > b2
      = True
    | (A5, b1,  _,  _) <- c1
    , (A3, b2,  _, D5) <- c2
    , b1 > b2
      = True
    | otherwise
      = False

rr3b :: RRule
rr3b c1 c2
    | (A5, b1,  _,  _) <- c1
    , ( _, b2,  C2, _) <- c2
    , b2 > B2
    , b1 == pred (pred b2)
      = True
    | otherwise
      = False
