-- The Oregon Trail (Logic Problems, Issue No. 388 2017, page 30)
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
    unless (null ms)
        putStrLn "Unresolved"
        putStrLn "----------"
        mapM_ (\s -> putStrLn $ show s ++ "\n") ms
  where
    (cs, ms) = solveLogicProblem rules [rr3a, rr3b]

-- Describe elements
instance Show A where
    show = showFrom [ "1933"
                    , "1934"
                    , "1936"
                    , "1937"
                    , "1938" ]
instance Show B where
    show = showFrom [ "Algeria"
                    , "Borneo"
                    , "Peru"
                    , "Siam"
                    , "Tibet" ]
instance Show C where
    show = showFrom [ "Cavern of Silver"
                    , "Elixir of Life"
                    , "Fabulous diamond"
                    , "Golden Statuette"
                    , "Lost City" ]
instance Show D where
    show = showFrom [ "Betty Kister"
                    , "Cher Perill"
                    , "Helen Highwater"
                    , "Rhoda Cammell"
                    , "Shona Mapp" ]

-- Absolute rules
rules :: Rule
rules = r1 . r2 . r3 . r4 . r5 . r6
  where
    r1 = (A1 <-> B1) . (A1 >|< D3) . (A2 <-> D2)
    r2 = A3 <-> C1
    r3 = (B5 >|< D5) . (B2 >|< C5) . (B5 >|< A1) . (B5 >|< A4) . (D5 >|< A1) . (D5 >|< A2) . (D5 >|< A4) . (B2 >|< A1) . (B2 >|< A3) . (C5 >|< A2) . (C5 >|< A5)
    r4 = B4 <-> D4
    r5 = D1 <-> C3
    r6 = B3 <-> C4

-- Relative rules
rr3a :: RRule
rr3a c1 c2
    | (a1, B5,  _,  _) <- c1
    , (a2,  _,  _, D5) <- c2
    , case a2 of
          A3 -> a1 == A2
          A5 -> a1 == A3
          _  -> False
      = True
    | otherwise
      = False

rr3b :: RRule
rr3b c1 c2
    | (a1, B2,  _,  _) <- c1
    , (a2,  _, C5,  _) <- c2
    , case a2 of
          A1 -> a1 == A2
          A3 -> a1 == A4
          A4 -> a1 == A5
          _  -> False
      = True
    | otherwise
      = False
