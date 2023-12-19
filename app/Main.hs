module Main (main) where

import Lib (dealCardsRandom, possibleSets)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.Random (mkStdGen)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [c, v, p] ->
      do
        let g = mkStdGen 42
            dealtCards = dealCardsRandom (read c) (read v) (read p) g
        print $ possibleSets (dealtCards) (read v)
    _ -> do
      pn <- getProgName
      die $ "Usage: " ++ pn ++ " <cards dealt> <number of values> <number of traits>"
