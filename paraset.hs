import Data.Set (Set)
import qualified Data.Set as Set

import Data.Map (Map)
import qualified Data.Map as Map

import Data.List (sort)

{-
cards are represented as lists, where the index represents the trait, and 
c[i] represents the value of trait i
-}
type Card = [Int]

{- 
main function: given parameters 
  C: number of cards dealt,  
  p: number of traits
  v: number of values per trait,
-}
possibleSetCount :: Int -> Int -> Int -> Int
possibleSetCount c v p =
  let
    dealtCards = dealCards 12 4 3
    preSets = generatePreSets dealtCards
  in
    length $ filter id $ map (\x -> isSetPossible dealtCards x) preSets


{- 
  hard coded for now so we can check 
-}
dealCards :: Int -> Int -> Int -> [Card]
dealCards c v p = sort
  [
    [3,3,3,1], [2,1,1,2], [1,2,3,2], [2,2,2,3],
    [1,2,2,3], [3,1,1,3], [2,1,2,2], [3,3,3,3],
    [2,2,2,1], [1,1,1,2], [1,2,1,1], [2,1,3,2]
  ]

-- https://stackoverflow.com/questions/52602474/function-to-generate-the-unique-combinations-of-a-list-in-haskell 
generatePreSets :: [Card] -> Int -> [[Card]]
generatePreSets dealtCards v = generatePreSets' dealtCards v-1
  where generatePreSets' _ 0 = [[]]
        generatePresets' [] _ = []
        generatePresets' (x:xs) n = map (x:) (generatePreSets' xs (n-1)) ++ generatePreSets' xs n


{-
  checks if a valid, correctly ordered set is possible 
-}
isValidSetPossible :: Set Card -> [Card]  -> Bool
isValidSetPossible dealtCards preSet =
  case getMissingCard preSet of
    Nothing -> False
    Just missingCard -> member missingCard dealtCards && (isOrderedSet preSet missingCard)
  where isOrderedSet pS mC = and (zipWith (>=) (head pS) mC)

getMissingValue :: Int -> [Int] -> Maybe Int
getMissingValue v values@(firstVal : _)
  | Map.size m == 1 = Just firstVal
  | all (==1) (Map.elems m) = Just $ (v*(v+1) `div` 2) - sum (Map.keys m)
  | otherwise = Nothing
  where
      m = Map.fromListWith (+) [(val, 1) | val <- values]

getMissingCard :: [Card] -> Int -> Maybe Card
getMissingCard preSet v =
  mapM (getMissingValue v) transposedList
  where
    transpose :: [[a]]->[[a]]
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)
    transposedList = transpose preSet
