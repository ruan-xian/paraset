import Data.Map (Map)
import qualified Data.Map as Map
getMissingValue :: Int -> [Int] -> Maybe Int
getMissingValue v values@(firstVal : _)
  | Map.size m == 1 = Just firstVal
  | all (==1) (Map.elems m) = Just $ (v*(v+1) `div` 2) - sum (Map.keys m)
  | otherwise = Nothing
  where
      m = Map.fromListWith (+) [(val, 1) | val <- values]

getMissingCard :: [[Int]] -> Int -> Maybe [Int]
getMissingCard preSet v =
  mapM (getMissingValue v) transposedList
  where
    transpose :: [[a]]->[[a]]
    transpose ([]:_) = []
    transpose x = map head x : transpose (map tail x)
    transposedList = transpose preSet