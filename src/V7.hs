module V7 (possibleSets) where

import Control.DeepSeq (force)
import Control.Parallel.Strategies (parBuffer, rseq, using)
import Data.Bits (Bits (bit, shiftR, testBit, (.&.), (.|.)))
import Data.Maybe (catMaybes)
import Data.Set (Set)
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
  let c = length dealtCards
      bitStrings = getBitstrings c (v - 1) dealtCards
   in catMaybes (bitStrings `using` parBuffer 5000 rseq)

bitStringToMaybeSet :: [Card] -> Set Card -> Int -> Integer -> Maybe [Card]
bitStringToMaybeSet dealtCards dealtCardsSet v bitstring =
  getPossibleSet dealtCardsSet v $ bitStringToPreset dealtCards bitstring

getBitstrings :: Int -> Int -> [Card] -> [Maybe [Card]]
getBitstrings n k dealtCards = map (force $ bitStringToMaybeSet dealtCards (Set.fromList dealtCards) (k + 1)) $ takeWhile (< bit (n + 1)) $ iterate next (bit k - 1)
  where
    next x =
      let smallest = x .&. negate x
          ripple = x + smallest
          new_smallest = ripple .&. negate ripple
       in ripple .|. (((new_smallest `div` smallest) `shiftR` 1) - 1)

bitStringToPreset :: [Card] -> Integer -> [Card]
bitStringToPreset dealtCards bitstring =
  [x | (x, i) <- zip dealtCards [0 ..], testBit bitstring i]
