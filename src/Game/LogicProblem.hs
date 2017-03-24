{-|
Module      : Game.LogicProblem
Description : Solve 4-category 5-element logic problem puzzles
Copyright   : (c) Mike Pilgrem 2017
License     : BSD3
Maintainer  : public@pilgrem.com
Stability   : experimental
-}
module Game.LogicProblem
    ( -- * Types
      A (..)
    , B (..)
    , C (..)
    , D (..)
    , Combination
    , Rule
    , RRule
      -- * Functions
    , showFrom
    , (<->)
    , (>|<)
    , solveLogicProblem
    ) where

import Data.Array
import Data.List (foldl')
import Data.Maybe (isNothing)

import Game.LogicProblem.Types.C4E5
import Game.LogicProblem.Types.Internal

-- | Seek to solve puzzle based on basic rules and any relative rules. The
-- result is a pair of lists. The first list is of valid 'Combination's. The
-- second list is of any 'Combination's which may be valid but have not been
-- identified as certainly valid. The second list will be empty if the puzzle
-- has been solved.
solveLogicProblem
    :: Rule    -- ^ The basic rules.
    -> [RRule] -- ^ A list (which may be empty) of relative rules.
    -> ([Combination], [Combination])
solveLogicProblem rule rRules = (validCombos, undecidedCombos)
  where
    validCombos     = map comboToCombination $ allValidCombos step2
    undecidedCombos = map comboToCombination $ allUndecidedCombos step2
    step1           = applyRule rule ignorance
    step2           = applyRRules rRules step1

-- | Apply a basic rule to the universe and then apply first and second level
-- simplification.
applyRule :: Rule -> Universe -> Universe
applyRule rule x = simplify2 $ simplify1 $ x // rule []

-- | Apply a list of relative rules to the universe.
applyRRules' :: [RRule] -> Universe -> Universe
applyRRules' rRules x = foldl' f x rRules
  where
    f x' rr = simplifyrr rr x'

-- | Apply a list of relative rules to the universe recursively, until it has no
-- effect on the number of undecided combinations.
applyRRules :: [RRule] -> Universe -> Universe
applyRRules rRules x = if c' < c
                    then applyRRules rRules x'
                    else x
  where
    c  = length $ allUndecidedCombos x
    c' = length $ allUndecidedCombos x'
    x' = applyRRules' rRules x

-- | A list of all the elements in a category.
allEInCat :: [Int]
allEInCat = [0 .. 4]

-- | A list of all possible combinations of elements.
allCombos :: [Combo]
allCombos = [ (a, b, c, d) | a <- allEInCat
                           , b <- allEInCat
                           , c <- allEInCat
                           , d <- allEInCat ]

-- | A list of all possible elememts.
allElements :: [Element]
allElements = [E cat e | cat <- [A .. D]
                       , e   <- allEInCat]

-- | A list of all possible (unordered) pairs of elememnts.
allPairs :: [(Element, Element)]
allPairs = [ (E cat1 e1, E cat2 e2) | cat1 <- [A .. C]
                                    , cat2 <- [(succ cat1) .. D]
                                    , e1 <- allEInCat
                                    , e2 <- allEInCat ]

-- | Is the element included in the combination?
incl :: Combo -> Element -> Bool
incl (a, b, c, d) (E cat e) = e == case cat of
    A -> a
    B -> b
    C -> c
    D -> d

-- | Is the element excluded from (not included in) the combination?
excl :: Combo -> Element -> Bool
excl combo e = not $ incl combo e

-- | Create an update from a state and a combination.
to :: State -> Combo -> Update
to s combo = (combo, s)

-- | Create a list of updates from a state and a list of combinations.
mapTo :: State -> [Combo] -> [Update]
mapTo s = map (to s)

-- | The starting state of the universe is ignorance: all possible combinations
-- of elements are undecided.
ignorance :: Universe
ignorance = array ((0, 0, 0, 0), (4, 4, 4, 4)) (mapTo Nothing allCombos)

-- | Create updates for a linked pair of elements. All combinations that
-- include one element but exclude the other are assumed to be invalid.
link :: (Element, Element) -> [Update]
link (e1, e2) = updates
  where
    updates  = mapTo (Just False) combos
    combos = filter criteria allCombos
    criteria combo =  (incl combo e1 && excl combo e2)
                   || (excl combo e1 && incl combo e2)

-- | Generate basic rule establishing a direct connection between a specific
-- element of one category and one of another category.
(<->) :: (IsCategory c1, IsCategory c2) => c1 -> c2 -> Rule
e1 <-> e2 = (++ updates)
  where
    updates  = link (toE e1, toE e2)

unlink :: Element -> Element -> [Update]
unlink e1 e2 = updates
  where
    updates  = mapTo (Just False) combos
    combos = filter criteria allCombos
    criteria combo = incl combo e1 && incl combo e2

-- | Generate basic rule establishing no connection between a specific
-- element of one IsCategory and one of another IsCategory.
(>|<) :: (IsCategory c1, IsCategory c2) => c1 -> c2 -> Rule
e1 >|< e2 = (++ updates)
  where
    updates = unlink (toE e1) (toE e2)

-- | Updates for a valid combination. The combination is updated to valid and
-- all other combinations involving the elements of the combination are invalid.
solve' :: Combo -> [Update]
solve' solvedCombo = updates
  where
    updates = mapTo (Just False) combos ++ [to (Just True) solvedCombo]
    combos = filter criteria allCombos
    criteria combo = 
        (incl combo a' || incl combo b' || incl combo c' || incl combo d') &&
        combo /= solvedCombo
    (a', b', c', d') = comboToElements solvedCombo

-- | If not already a valid combination in the universe, generates updates for a
-- valid combination.
solve :: Universe -> Combo -> [Update]
solve x solvedCombo =
    if x ! solvedCombo == Just True
        then []
        else solve' solvedCombo

-- | All the undecided combinations in the universe of combinations.
allUndecidedCombos :: Universe -> [Combo]
allUndecidedCombos x = filter (isUndecidedCombo x) allCombos

-- | Is the validity of the combination undecided in the universe of
-- combinations?
isUndecidedCombo :: Universe -> Combo -> Bool
isUndecidedCombo x combo = isNothing (x ! combo)

-- | A list of all the valid combinations in the universe of combinations.
allValidCombos :: Universe -> [Combo]
allValidCombos x = filter (isValidCombo x) allCombos

-- | Is the combination valid in the universe of combinations?
isValidCombo :: Universe -> Combo -> Bool
isValidCombo x combo = (x ! combo) == Just True

-- | A list of all non-invalid combinations in the universe of combinations.
allPossibleCombos :: Universe -> [Combo]
allPossibleCombos x = allValidCombos x ++ allUndecidedCombos x

-- | A list of all combinations that include the specified element.
hasElement :: [Combo] -> Element -> [Combo]
hasElement combos e = filter (`incl` e) combos

-- | Does the list of combinations have only one that includes the element?
uniquelyHasElement :: [Combo] -> Element -> Bool
uniquelyHasElement combos e =
    case hasElement combos e of
        [_] -> True
        _   -> False

-- | A list of combinations that are unique in respect of including one of the
-- possible elements.
uniqueCombos :: [Combo] -> [Combo]
uniqueCombos combos = concatMap (hasElement combos) $ filter (uniquelyHasElement combos) allElements 

-- | Apply first level of simplification recursively, until it produces no
-- updates.
simplify1 :: Universe -> Universe
simplify1 x = case updates of
                 [] -> x
                 _  -> simplify1 $ x // updates
  where
    updates = simplify1' x

-- | Apply first level of simplifcation. For any unique undecided combinations,
-- assume that the combination is a valid one.
simplify1' :: Universe -> [Update]
simplify1' x = updates
  where
    updates = concatMap solve' (uniqueCombos combos)
    combos  = allUndecidedCombos x

hasPair :: [Combo] -> (Element, Element) -> Bool
hasPair combos (e1, e2) = any criteria combos
  where
    criteria combo = incl combo e1 && incl combo e2

uniquelyHasPair :: [Combo] -> (Element, Element) -> Bool
uniquelyHasPair combos (e1, e2) =  hasPair combos (e1, e2)
                                && not otherPair
  where
    E cat1 i1 = e1
    E cat2 i2 = e2
    otherPair = any (hasPair combos) (otherPair1 ++ otherPair2)
    otherPair1 = [ (e1, E cat2 i2') | i2' <- allEInCat
                                    , i2' /= i2
                 ]
    otherPair2 = [ (E cat1 i1', e2) | i1' <- allEInCat
                                    , i1' /= i1
                 ]

uniquePairs :: [Combo] -> [(Element, Element)]
uniquePairs combos = filter (uniquelyHasPair combos) allPairs

simplify2' :: Universe -> [Update]
simplify2' x = updates
  where
    updates = concatMap link (uniquePairs combos)
    combos = allUndecidedCombos x

-- | Apply second level simplifcation recursively until it has no effect on the
-- number of undecided combinations.
simplify2 :: Universe -> Universe
simplify2 x = if c' < c
                  then simplify2 x'
                  else x
  where
    c  = length $ allUndecidedCombos x
    c' = length $ allUndecidedCombos x'
    x' = x // updates
    updates = simplify2' x

-- | For a list of combinations and a given combination, does the relative rule
-- apply?
rRAppliesForCombo :: RRule -> [Combo] -> Combo -> Bool
rRAppliesForCombo rr combos combo = any (rr' combo) combos
  where
    rr' c1 c2 = rr (comboToCombination c1) (comboToCombination c2)

-- | For a list of combinations and a given combination, does the inverse of the
-- relative rule apply?
rRAppliesForCombo' :: RRule -> [Combo] -> Combo -> Bool
rRAppliesForCombo' rr combos combo = any (`rr'` combo) combos
  where
    rr' c1 c2 = rr (comboToCombination c1) (comboToCombination c2)

-- | List possibly valid combinations for which the relative rule applies.
allRrApplies :: RRule -> Universe -> [Combo]
allRrApplies rr x = filter (rRAppliesForCombo rr combos) combos
  where
    combos = allPossibleCombos x

-- | List possibly valid combinations for which the inverse of the relative rule
-- applies.
allRrApplies' :: RRule -> Universe -> [Combo]
allRrApplies' rr x = filter (rRAppliesForCombo' rr combos) combos
  where
    combos = allPossibleCombos x

-- | Simplify the universe of combinations given a relative rule, using the rule
-- and its inverse. If a single possible combination works, it is assumed to be
-- a valid combination. If all possible combinations that work share a pair of
-- elements, then that pair is assumed to be linked. First and second levels of
-- simplification are then applied.
simplifyrr :: RRule -> Universe -> Universe
simplifyrr rr x = simplify2 $ simplify1 $ x // updates
  where
    updatesRr  = case allRrApplies rr x of
                     []      -> []
                     [combo] -> solve x combo
                     combos  -> concatMap link $ allHasPairs combos
    updatesRr' = case allRrApplies' rr x of
                     []      -> []
                     [combo] -> solve x combo
                     combos  -> concatMap link $ allHasPairs combos
    updates = updatesRr ++ updatesRr'

-- | Is a pair of elements present in all of the combinations?
allHasPair :: [Combo] -> (Element, Element) -> Bool
allHasPair combos (e1, e2) = all criteria combos
  where
    criteria combo = incl combo e1 && incl combo e2

-- | A list of pairs of elements that are present in all of the combinations.
allHasPairs :: [Combo] -> [(Element, Element)]
allHasPairs combos = [pair | pair <- allPairs
                           , allHasPair combos pair]    
