module V6Chunks (possibleSets) where

import Algo2Base (bitStringToMaybeSet, getBitstrings)
import Control.Parallel.Strategies (parMap, rseq)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)
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
      bitStringChunks = chunksOf 5000 $ getBitstrings c (v - 1)
   in concat $
        parMap
          rseq
          (mapMaybe (bitStringToMaybeSet dealtCards (Set.fromList dealtCards) v))
          bitStringChunks
