module V1 (possibleSets) where

import Algo1Base (generatePreSets)
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
  let preSets = generatePreSets v dealtCards
   in mapMaybe (getPossibleSet (Set.fromList dealtCards) v) preSets
