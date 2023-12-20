module Algo1Base (generatePreSets) where

import ParsetBase (Card)

{-
https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
faster ones here https://stackoverflow.com/questions/26727673/haskell-comparison-of-techniques-for-generating-combinations
    are not great bc `subsequences` can get really huge if `length dealtCards` is large
-}
generatePreSets :: Int -> [Card] -> [[Card]]
generatePreSets v = generatePreSets' (v - 1)
  where
    generatePreSets' 0 _ = [[]]
    generatePreSets' _ [] = []
    generatePreSets' n (x : xs) = map (x :) (generatePreSets' (n - 1) xs) ++ generatePreSets' n xs
