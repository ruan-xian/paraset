module V6Naive (possibleSets) where

import Algo2Base (bitStringToMaybeSet, getBitstrings)
import Control.Parallel.Strategies (parMap, rseq)
import Data.Maybe (catMaybes)
import Data.Set qualified as Set
import ParsetBase (Card)

{-
main function: given parameters
  C: number of cards dealt,
  v: number of values per trait,
  p: number of traits
  g: a random number generator
-}
possibleSets :: [Card] -> Int -> [[Card]]
possibleSets dealtCards v =
  let c = length dealtCards
   in catMaybes $
        parMap
          rseq
          (bitStringToMaybeSet dealtCards (Set.fromList dealtCards) v)
          (getBitstrings c (v - 1))
