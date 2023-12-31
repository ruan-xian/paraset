module V4
  ( dealCardsRandom,
    possibleSets,
    generateCardFromIndex,
    generateIndexFromCard
  )
where

import Control.Parallel.Strategies (parMap, rseq)
import Data.List (sort)
import Data.List.Split (chunksOf)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import System.Random (Random (randomR), StdGen)

{-
cards are represented as lists, where the index represents the trait, and
c[i] represents the value of trait i
-}
type CardIndex = Integer

type Card = [Int]

{-
main function: given parameters
  C: number of cards dealt,
  v: number of values per trait,
  p: number of traits
  g: a random number generator
-}
possibleSets :: [CardIndex] -> Int -> Int -> [[Card]]
possibleSets dealtCards v p =
  let preSets = generatePreSets v dealtCards
      --  in mapMaybe (getPossibleSet (Set.fromList dealtCards) v) preSets
      preSetChunks = chunksOf 10000 preSets
   in concat $ parMap rseq (mapMaybe (getPossibleSet (Set.fromList dealtCards) v p)) preSetChunks

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
constructRandomList :: Int -> Int -> Int -> Int -> StdGen -> [(CardIndex, CardIndex)]
constructRandomList c v p sofar gen
  | c == sofar = []
  | otherwise =
      (fromIntegral sofar, num) : constructRandomList c v p (sofar + 1) newGen
  where
    (num, newGen) = randomR (fromIntegral sofar, fromIntegral $ v ^ p - 1) gen

-- This performs all swaps and constructs the list of returned cards.
constructCards :: Int -> Int -> Int -> [(CardIndex, CardIndex)] -> Map.Map CardIndex CardIndex -> [CardIndex]
constructCards _ _ _ [] _ = []
constructCards c v p ((cardPosition, cardIndex) : nextCards) foundNums =
  cardInSwapPos : nextCardList
  where
    cardInCurrentPos = Map.findWithDefault cardPosition cardPosition foundNums
    cardInSwapPos = Map.findWithDefault cardIndex cardIndex foundNums
    nextCardList = constructCards c v p nextCards (Map.insert cardIndex cardInCurrentPos foundNums)

-- This transforms the card from its index into its list form.
generateCardFromIndex :: Int -> Int -> CardIndex -> Card
generateCardFromIndex _ 0 _ = []
generateCardFromIndex v remainingP index =
  fromInteger remIndex + 1 : generateCardFromIndex v (remainingP - 1) num
  where
    (num, remIndex) = quotRem index $ fromIntegral v

generateIndexFromCard :: Int -> Card -> CardIndex
generateIndexFromCard _ [] = 0
generateIndexFromCard v (x : xs) =
  (fromIntegral x - 1) + fromIntegral v * generateIndexFromCard v xs

-- Calls all necessary functions. Should probably be refactored to use a random seed (would need to become IO monad).
dealCardsRandom :: Int -> Int -> Int -> StdGen -> [CardIndex]
dealCardsRandom c v p g =
  sort $ constructCards c v p (constructRandomList c v p 0 g) Map.empty

{-
https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell
faster ones here https://stackoverflow.com/questions/26727673/haskell-comparison-of-techniques-for-generating-combinations
    are not great bc `subsequences` can get really huge if `length dealtCards` is large
-}
generatePreSets :: Int -> [CardIndex] -> [[CardIndex]]
generatePreSets v = generatePreSets' (v - 1)
  where
    generatePreSets' 0 _ = [[]]
    generatePreSets' _ [] = []
    generatePreSets' n (x : xs) = map (x :) (generatePreSets' (n - 1) xs) ++ generatePreSets' n xs

{-
  checks if a valid, correctly ordered set is possible
-}
getPossibleSet :: Set CardIndex -> Int -> Int -> [CardIndex] -> Maybe [Card]
getPossibleSet dealtCards v p preSet =
  case getMissingCard preSet v p of
    Nothing -> Nothing
    Just missingCard ->
      if Set.member (generateIndexFromCard v missingCard) dealtCards && generateIndexFromCard v missingCard < head preSet
        then Just $ missingCard : map (generateCardFromIndex v p) preSet
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

getMissingCard :: [CardIndex] -> Int -> Int -> Maybe Card
getMissingCard preSet v p =
  mapM (getMissingValue v) transposedList
  where
    transpose :: [[a]] -> [[a]]
    transpose ([] : _) = []
    transpose x = map head x : transpose (map tail x)
    transposedList = transpose (map (generateCardFromIndex v p) preSet)

-- https://stackoverflow.com/questions/2578930/understanding-this-matrix-transposition-function-in-haskell