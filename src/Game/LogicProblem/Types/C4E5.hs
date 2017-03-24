{-|
Module      : Game.LogicProblem.Types.C4E5
Description : Types for a 4-category 5-element logic problem puzzle
Copyright   : (c) Mike Pilgrem 2017
License     : BSD3
Maintainer  : public@pilgrem.com
Stability   : experimental
-}
module Game.LogicProblem.Types.C4E5
    ( IsCategory (..)
    , A (..)
    , B (..)
    , C (..)
    , D (..)
    , Combination
    , Rule
    , RRule
    , showFrom
    , combinationToCombo
    , comboToCombination
    )
    where

import Game.LogicProblem.Types.Internal

-- | A generic IsCategory with five elements, A1 to A5.
data A = A1 | A2 | A3 | A4 | A5
    deriving (Enum, Eq, Ord)
-- | A generic IsCategory with five elements, B1 to B5.
data B = B1 | B2 | B3 | B4 | B5
    deriving (Enum, Eq, Ord)
-- | A generic IsCategory with five elements, C1 to C5.
data C = C1 | C2 | C3 | C4 | C5
    deriving (Enum, Eq, Ord)
-- | A generic IsCategory with five elements, D1 to D5.
data D = D1 | D2 | D3 | D4 | D5
    deriving (Enum, Eq, Ord)

-- | A possible combination of elements, one from each of the four categories.
type Combination = (A, B, C, D)

-- | A basic rule either specifies a direct connection between a specific
-- element of one category and a specific element of another category (see
-- '<->') or a lack of connection between them (see '>|<'). Basic rules are
-- combined by function composition (a basic rule is a function that adds
-- further updates to any existing updates to our initial knowledge). For
-- example:
--
-- > rules :: Rule
-- > rules = (A1 <-> B1) . (A2 >|< B2)
--
-- encodes that A1 and B1 are connected and that A2 and B2 are not connected.
type Rule = [Update] -> [Update]

-- | A relative rule specifies whether or not two 'Combination's of elements are
-- related in a specified way. For example:
--
-- > rRule :: RRule
-- > rRule c1 c2
-- >     | (a1, B1,  _,  _) <- c1
-- >     , (a2,  _, C1,  _) <- c2
-- >     , a2 \= A5
-- >     , a1 == succ a2
-- >       = True
-- >     | otherwise
-- >       = False
--
-- encodes that the A element of B1 immediately follows that of C1. To avoid
-- out-of-bounds errors, it is necessary to specify that the A element of C1 is
-- not the last one (A5).
type RRule = Combination -> Combination -> Bool

-- | A helper function to show elements of categories. The names of the five
-- elements are provided as a list of five strings. For example:
--
-- > instance Show A where
-- >     show = showFrom [ "Monday"
-- >                     , "Tuesday"
-- >                     , "Wednesday"
-- >                     , "Thursday"
-- >                     , "Friday" ]
--
-- encodes that the five elements of IsCategory 'A' are named after weekdays.
showFrom :: (Enum c) => [String] -> c -> String
showFrom names e = paddedNames !! fromEnum e
  where
    paddedNames = names ++ [msg, msg, msg, msg, msg]
    msg         = "Undefined"

-- Bridge from 'external' to 'internal' types

class (Enum c) => IsCategory c where
    category :: c -> Category

    toE :: c -> Element
    toE e = E (category e) (fromEnum e)

instance IsCategory A where
    category _ = A
instance IsCategory B where
    category _ = B
instance IsCategory C where
    category _ = C
instance IsCategory D where
    category _ = D

combinationToCombo :: Combination -> Combo
combinationToCombo (a, b, c, d) = ( fromEnum a
                                  , fromEnum b
                                  , fromEnum c
                                  , fromEnum d
                                  )

comboToCombination :: Combo -> Combination
comboToCombination (a, b, c, d) = ( toEnum a
                                  , toEnum b
                                  , toEnum c
                                  , toEnum d)
