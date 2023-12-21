module Algo2Base (getBitstrings, bitStringToMaybeSet) where

import Data.Bits
import Data.Set (Set)
import ParsetBase (Card, getPossibleSet)

bitStringToMaybeSet :: [Card] -> Set Card -> Int -> Integer -> Maybe [Card]
bitStringToMaybeSet dealtCards dealtCardsSet v bitstring =
  getPossibleSet dealtCardsSet v $ bitStringToPreset dealtCards bitstring

-- https://stackoverflow.com/questions/37745191/haskell-parallel-code-is-slower-than-sequential-version
getBitstrings :: Int -> Int -> [Integer]
getBitstrings n k = takeWhile (< bit (n + 1)) $ iterate next (bit k - 1)
  where
    next x =
      let smallest = x .&. negate x
          ripple = x + smallest
          new_smallest = ripple .&. negate ripple
       in ripple .|. (((new_smallest `div` smallest) `shiftR` 1) - 1)

bitStringToPreset :: [Card] -> Integer -> [Card]
bitStringToPreset dealtCards bitstring =
  [x | (x, i) <- zip dealtCards [0 ..], testBit bitstring i]
