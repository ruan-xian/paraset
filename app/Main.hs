module Main (main) where

import Lib (possibleSets)
import System.Environment (getArgs, getProgName)
import System.Exit (die)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [c, v, p] -> do
      print $ possibleSets (read c) (read v) (read p)
    _ -> do
      pn <- getProgName
      die $ "Usage: " ++ pn ++ " <cards dealt> <number of values> <number of traits>"