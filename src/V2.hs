module V2 (possibleSets) where

import Algo1Base (generatePreSets)
import Control.Parallel (par)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import ParsetBase (Card, getPossibleSet)

{-
main function: given parameters
  C: number of cards dealt,
  v: number of values per trait,
  p: number of traits
  g: a random number generator
-}
possibleSets :: [Card] -> Int -> [[Card]]
possibleSets dealtCards v =
  let preSets = generatePreSetsWithDepth v 5 dealtCards
   in mapMaybe (getPossibleSet (Set.fromList dealtCards) v) preSets

generatePreSetsWithDepth :: Int -> Int -> [Card] -> [[Card]]
generatePreSetsWithDepth v = generatePreSetsWithDepth' (v - 1)
  where
    generatePreSetsWithDepth' 0 _ _ = [[]]
    generatePreSetsWithDepth' _ _ [] = []
    generatePreSetsWithDepth' n 0 xs = generatePreSets (n + 1) xs
    generatePreSetsWithDepth' n d (x : xs) = ps1 `par` ps2 `seq` (map (x :) ps2 ++ ps1)
      where
        ps1 = generatePreSetsWithDepth' n (d - 1) xs
        ps2 = generatePreSetsWithDepth' (n - 1) (d - 1) xs
