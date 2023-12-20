module V3 (possibleSets) where

import Algo1Base (generatePreSets)
import Control.Parallel.Strategies (parMap, rseq)
import Data.List.Split (chunksOf)
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
      preSetChunks = chunksOf 10000 preSets
   in concat $ parMap rseq (mapMaybe (getPossibleSet (Set.fromList dealtCards) v)) preSetChunks
