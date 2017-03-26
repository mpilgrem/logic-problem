-- Online sample logic problem at http://www.puzzler.com/FileDepository/Puzzles/Samples/LogicProblem.pdf
-- Status: Puzzle solved

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
    (cs, ms) = solveLogicProblem rules [rr1, rr3]

-- Describe elements
instance Show A where
    show = showFrom [ "Citizen Canine"
                    , "Donnie Barko"
                    , "Hello, Collie!"
                    , "Romancing the Bone"
                    , "Spinal Yap" ]
instance Show B where
    show = showFrom [ "Biopic"
                    , "Comedy"
                    , "Musical"
                    , "Period Drama"
                    , "Thriller" ]
instance Show C where
    show = showFrom [ "85 minutes"
                    , "95 minutes"
                    , "105 minutes"
                    , "110 minutes"
                    , "125 minutes" ]
instance Show D where
    show = showFrom [ "Best Actor"
                    , "Best Director"
                    , "Best Make-up"
                    , "Best Movie"
                    , "Best Screenplay" ]

-- Absolute rules
rules :: Rule
rules = r1 . r2 . r3 . r4 . r5 . r6
  where
    r1 = (A2 <-> B1) . (A2 >|< C1) . (A2 >|< C2) . (A2 >|< C5) . (A5 <-> D5) .
        (A5 >|< C1) . (A5 >|< C2) . (A5 >|< C3)
    r2 = (C1 <-> D4) . (A3 >|< D4)
    r3 = (B2 >|< C1) . (B4 >|< C5)
    r4 = (B5 <-> C2) . (B5 >|< D2)
    r5 = A1 <-> C3
    r6 = D3 <-> B3

-- Relative rules
rr1 :: RRule
rr1 co1 co2
    | (A2,  _, c1,  _) <- co1
    , (A5,  _, c2,  _) <- co2
    , c1 < c2
      = True
    | otherwise
      = False

rr3 :: RRule
rr3 co1 co2
    | ( _, B2, c1,  _) <- co1
    , ( _, B4, c2,  _) <- co2
    , c1 > c2
      = True
    | otherwise
      = False
