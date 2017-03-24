-- Going To Pieces (Logic Problems, Issue No. 388 2017, page 32)
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
    (cs, ms) = solveLogicProblem rules [rr1a, rr1b, rr2a, rr2b, rr3a, rr3b, rr6]

-- Describe elements
instance Show A where
    show = showFrom [ "Elizabeth"
                    , "Eric"
                    , "Moira"
                    , "Norman"
                    , "Sandra" ]
instance Show B where
    show = showFrom [ "Blue Boy"
                    , "Laughing Cavalier"
                    , "Mona Lisa"
                    , "Sunflowers"
                    , "The Hay Wain" ]
instance Show C where
    show = showFrom [ "750 pieces"
                    , "1000 pieces"
                    , "1200 pieces"
                    , "1500 pieces"
                    , "2000 pieces" ]
instance Show D where
    show = showFrom [ "5 days"
                    , "6 days"
                    , "7 days"
                    , "8 days"
                    , "10 days" ]

-- Absolute rules
rules :: Rule
rules = r1 . r2 . r3 . r4 . r5 . r6 . r7
  where
    r1 = (A4 >|< D2) . (A4 >|< D1) . (B4 <-> D1) . (A4 >|< C1) . (D2 >|< C5) . (A4 >|< C5) . (D1 >|< C1)
    r2 = (A5 >|< B5) . (A5 >|< D1) . (B5 >|< D5) . (B5 >|< C1) . (A5 >|< C5)
    r3 = (D4 >|< B2) . (D4 >|< C1) . (B2 >|< C5) . (B2 >|< D5)
    r4 = A2 <-> C3
    r5 = C2 >|< D5
    r6 = (B1 >|< A3) . (B1 >|< C1) . (B1 >|< C2) . (B1 >|< C3) . (A3 >|< C3) . (A3 >|< C4) . (A3 >|< C5)
    r7 = A1 <-> D3

-- Relative rules
rr1a :: RRule
rr1a co1 co2
    | (A4,  _, c1,  _) <- co1
    , ( _,  _, c2, D2) <- co2
    , c1 > c2
      = True
    | otherwise
      = False

rr1b :: RRule
rr1b co1 co2
    | (A4,  _, c1,  _) <- co1
    , ( _, B4, c2, D1) <- co2
    , c1 < c2
      = True
    | otherwise
      = False

rr2a :: RRule
rr2a co1 co2
    | (A5,  _,  _, d1) <- co1
    , ( _, B5,  _, d2) <- co2
    , d1 > d2
      = True
    | otherwise
      = False

rr2b :: RRule
rr2b co1 co2
    | (A5,  _, c1,  _) <- co1
    , ( _, B5, c2,  _) <- co2
    , c2 > c1
      = True
    | otherwise
      = False

rr3a :: RRule
rr3a co1 co2
    | ( _,  _, c1, D4) <- co1
    , ( _, B2, c2,  _) <- co2
    , c2 < C5
    , c1 == succ c2
      = True
    | otherwise
      = False

rr3b :: RRule
rr3b co1 co2
    | ( _,  _,  _, D4) <- co1
    , ( _, B2,  _, d2) <- co2
    , d2 < D4
      = True
    | otherwise
      = False

rr6 :: RRule
rr6 co1 co2
    | ( _, B1, c1,  _) <- co1
    , (A3,  _, c2,  _) <- co2
    , case c1 of
          C4 -> c2 == C1
          C5 -> c2 == C2
          _  -> False
      = True
    | otherwise
      = False
