-- Gentle Giants (Logic Problems, Issue No. 388 2017, page 16)
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
    (cs, ms) = solveLogicProblem rules []

-- Describe elements
instance Show A where
    show = showFrom [ "Boneshaker"
                    , "Bulldozer"
                    , "Mighty Monty"
                    , "The Wrench"
                    , "Wringer Watson" ]
instance Show B where
    show = showFrom [ "Mon"
                    , "Wed"
                    , "Thurs"
                    , "Fri"
                    , "Sat" ]
instance Show C where
    show = showFrom [ "Bird watching"
                    , "Cake decorating"
                    , "Flower arranging"
                    , "Fostering puppies"
                    , "Pottery" ]
instance Show D where
    show = showFrom [ "5"
                    , "8"
                    , "11"
                    , "12"
                    , "14" ]

-- Absolute rules
rules :: Rule
rules = r1 . r2 . r3 . r4 . r5 . r6
  where
    r1 = A4 <-> D4
    r2 = (A3 <-> C2) . (A3 >|< D1) . (A3 >|< D3) . (A3 >|< B5) 
    r3 = C3 <-> B1
    r4 = (C4 <-> D2) . (C4 >|< A2) . (A2 <-> B3)
    r5 = (B4 <-> D1) . (B4 >|< A1)
    r6 = A5 >|< C5

-- Relative rules
