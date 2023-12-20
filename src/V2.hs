module V2
  ( dealCardsRandom,
    possibleSets,
  )
where

import Control.Parallel (par)
import Data.List (sort)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Random (Random (randomR), StdGen)

{-
cards are represented as lists, where the index represents the trait, and
c[i] represents the value of trait i
-}
type Card = [Int]

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

{-
  We generate c randomly dealt cards through generating random "swaps".
  The initial "deck" consists of all cards in sorted order (here, each card
  is represented as a single integer from 0 to v^p - 1, instead of its constituent parts.)
  Then we generate c "swaps", where for each of the first c positions in the deck,
  we swap the card with any subsequent card in the deck. After performing all swaps, we
  return the first c cards in the deck.

  This approach is inspired by https://wiki.haskell.org/Random_shuffle (Drawing without replacement).
-}

-- This generates all c swaps.
constructRandomList :: Int -> Int -> Int -> Int -> StdGen -> [(Int, Int)]
constructRandomList c v p sofar gen
  | c == sofar = []
  | otherwise =
      (sofar, num) : constructRandomList c v p (sofar + 1) newGen
  where
    (num, newGen) = randomR (sofar, v ^ p - 1) gen

-- This performs all swaps and constructs the list of returned cards.
constructCards :: Int -> Int -> Int -> [(Int, Int)] -> Map.Map Int Int -> [Card]
constructCards _ _ _ [] _ = []
constructCards c v p ((cardPosition, cardIndex) : nextCards) foundNums =
  generateCardFromIndex v p cardInSwapPos : nextCardList
  where
    cardInCurrentPos = Map.findWithDefault cardPosition cardPosition foundNums
    cardInSwapPos = Map.findWithDefault cardIndex cardIndex foundNums
    nextCardList = constructCards c v p nextCards (Map.insert cardIndex cardInCurrentPos foundNums)

-- This transforms the card from its index into its list form.
generateCardFromIndex :: Int -> Int -> Int -> Card
generateCardFromIndex _ 0 _ = []
generateCardFromIndex v remainingP index =
  remIndex + 1 : generateCardFromIndex v (remainingP - 1) num
  where
    (num, remIndex) = quotRem index v

-- Calls all necessary functions. Should probably be refactored to use a random seed (would need to become IO monad).
dealCardsRandom :: Int -> Int -> Int -> StdGen -> [Card]
dealCardsRandom c v p g =
  sort $ constructCards c v p (constructRandomList c v p 0 g) Map.empty

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

generatePreSetsWithDepth :: Int -> Int -> [Card] -> [[Card]]
generatePreSetsWithDepth v = generatePreSetsWithDepth' (v - 1)
  where
    generatePreSetsWithDepth' 0 _ _ = [[]]
    generatePreSetsWithDepth' _ _ [] = []
    --     generatePreSets' n (x : xs) = map (x :) (generatePreSets' (n - 1) xs) ++ generatePreSets' n xs

    -- generatePreSets' n (x : xs) =
    --   let preImage1 = generatePreSets' (n - 1) xs
    --       preImage2 = generatePreSets' n xs
    --    in preImage2 `par` (map (x :) preImage1 ++ preImage2)

    generatePreSetsWithDepth' n 0 xs = generatePreSets (n + 1) xs
    generatePreSetsWithDepth' n d (x : xs) = ps1 `par` ps2 `seq` (map (x :) ps2 ++ ps1)
      where
        ps1 = generatePreSetsWithDepth' n (d - 1) xs
        ps2 = generatePreSetsWithDepth' (n - 1) (d - 1) xs

{-
  checks if a valid, correctly ordered set is possible
-}
getPossibleSet :: Set Card -> Int -> [Card] -> Maybe [Card]
getPossibleSet dealtCards v preSet =
  case getMissingCard preSet v of
    Nothing -> Nothing
    Just missingCard ->
      if Set.member missingCard dealtCards && missingCard < head preSet
        then Just $ missingCard : preSet
        else Nothing

getMissingValue :: Int -> [Int] -> Maybe Int
getMissingValue v values
  | Map.size m == 1 = Just $ head values
  | all eqOne (Map.elems m) = Just $ (v * (v + 1) `div` 2) - sum (Map.keys m)
  | otherwise = Nothing
  where
    eqOne :: Int -> Bool
    eqOne = (== 1)
    m = Map.fromListWith (+) [(val, 1) | val <- values]

getMissingCard :: [Card] -> Int -> Maybe Card
getMissingCard preSet v =
  mapM (getMissingValue v) transposedList
  where
    transpose :: [[a]] -> [[a]]
    transpose ([] : _) = []
    transpose x = map head x : transpose (map tail x)
    transposedList = transpose preSet

-- https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell