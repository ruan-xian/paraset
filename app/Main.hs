module Main (main) where

import Lib (dealCardsRandom, possibleSets)
import System.Console.GetOpt
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.Random (mkStdGen)

data Flag
  = Verbose -- -v
  | Help -- --help
  deriving (Eq, Ord, Enum, Show, Bounded)

flags :: [OptDescr Flag]
flags =
  [ Option
      ['v']
      []
      (NoArg Verbose)
      "Displays all output.",
    Option
      ['h']
      ["help"]
      (NoArg Help)
      "Print this help message"
  ]

main :: IO ()
main = do
  argv <- getArgs
  case getOpt RequireOrder flags argv of
    (args, [c, v, p], []) ->
      do
        let g = mkStdGen 42
            dealtCards = dealCardsRandom (read c) (read v) (read p) g
        print $ possibleSets dealtCards (read v)
    _ -> do
      pn <- getProgName
      die $ "Usage: " ++ pn ++ " [-v] <cards dealt> <number of values> <number of traits>"