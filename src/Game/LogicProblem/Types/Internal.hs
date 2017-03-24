{-|
Module      : Game.LogicProblem.Types.Internal
Description : Types for logic problem puzzles
Copyright   : (c) Mike Pilgrem 2017
License     : BSD3
Maintainer  : public@pilgrem.com
Stability   : experimental
-}
module Game.LogicProblem.Types.Internal
    ( Category (..)
    , Element (..)
    , Combo
    , State
    , Universe
    , Update
    , comboToElements
    ) where

import Data.Array (Array)

data Category = A | B | C | D
    deriving (Bounded, Enum, Eq, Ord, Show)

data Element = E Category Int
    deriving (Eq, Show)

instance Ord Element where
    compare (E c1 e1) (E c2 e2) =
        if c1 == c2
            then compare e1 e2
            else error "Attempting to compare elements from different \
                \categories."

type Combo = (Int, Int, Int, Int)

comboToElements :: Combo -> (Element, Element, Element, Element)
comboToElements (a, b, c, d) = (E A a, E B b, E C c, E D d)

type State = Maybe Bool

type Universe = Array Combo State

type Update = (Combo, State)
