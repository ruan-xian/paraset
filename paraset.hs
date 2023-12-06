import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

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
-}
possibleSets :: Int -> Int -> Int -> [[Card]]
possibleSets c v p =
  let dealtCards = dealCards c v p
      preSets = generatePreSets v dealtCards
   in mapMaybe (getPossibleSet (Set.fromList dealtCards) v) preSets

{-
human solution 
[2,1,1,2]  [2,1,2,2]  [2,1,3,2]
[1,2,1,1]  [1,2,2,3]  [1,2,3,2]
[1,2,1,1]  [2,1,2,2]  [3,3,3,3]
[1,1,1,2]  [2,2,2,3]  [3,3,3,1]
[1,2,2,3]  [2,1,1,2]  [3,3,3,1]
[1,1,1,2]  [2,2,2,1]  [3,3,3,3]
 -}

{-
code generated solutions
   [[1,2,1,1],[1,2,2,3],[1,2,3,2]], 2
   [[1,2,2,3],[2,1,1,2],[3,3,3,1]], 5
   [[2,1,1,2],[2,1,2,2],[2,1,3,2]], 1
   [[1,2,1,1],[2,1,2,2],[3,3,3,3]], 3
   [[1,1,1,2],[2,2,2,1],[3,3,3,3]], 6
   [[1,1,1,2],[2,2,2,3],[3,3,3,1]]] 4
-}

{-
  hard coded for now so we can check
-}
dealCards :: Int -> Int -> Int -> [Card]
dealCards c v p =
  sort
    [ [3, 3, 3, 1],
      [2, 1, 1, 2],
      [1, 2, 3, 2],
      [2, 2, 2, 3],
      [1, 2, 2, 3],
      [3, 1, 1, 3],
      [2, 1, 2, 2],
      [3, 3, 3, 3],
      [2, 2, 2, 1],
      [1, 1, 1, 2],
      [1, 2, 1, 1],
      [2, 1, 3, 2]
    ]

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

{-
  checks if a valid, correctly ordered set is possible
-}
getPossibleSet :: Set Card -> Int -> [Card] -> Maybe [Card]
getPossibleSet dealtCards v preSet@(firstCard : _) =
  case getMissingCard preSet v of
    Nothing -> Nothing
    Just missingCard ->
      if Set.member missingCard dealtCards && missingCard < firstCard
        then Just $ missingCard : preSet
        else Nothing

getMissingValue :: Int -> [Int] -> Maybe Int
getMissingValue v values@(firstVal : _)
  | Map.size m == 1 = Just firstVal
  | all (== 1) (Map.elems m) = Just $ (v * (v + 1) `div` 2) - sum (Map.keys m)
  | otherwise = Nothing
  where
    m = Map.fromListWith (+) [(val, 1) | val <- values]

getMissingCard :: [Card] -> Int -> Maybe Card
getMissingCard preSet v =
  mapM (getMissingValue v) transposedList
  where
    transpose :: [[a]] -> [[a]]
    transpose ([] : _) = []
    transpose x = map head x : transpose (map tail x)
    transposedList = transpose preSet
