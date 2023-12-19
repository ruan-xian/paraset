module Main (main) where

import Control.DeepSeq
import Control.Exception
import Lib (dealCardsRandom, possibleSets)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), die, exitSuccess, exitWith)
import System.IO
import System.Random (mkStdGen)

data Flag
  = Silent -- -s
  | Newline -- -n
  | Deck -- -d
  | Help -- --help
  deriving (Eq, Ord, Enum, Show, Bounded)

flags :: [OptDescr Flag]
flags =
  [ Option
      ['s']
      ["silent"]
      (NoArg Silent)
      "Silences all output.",
    Option
      ['d']
      ["deck"]
      (NoArg Deck)
      "Prints the deck generated.",
    Option
      ['n']
      ["newline"]
      (NoArg Newline)
      "Prints each solution on a separate line.",
    Option
      []
      ["help"]
      (NoArg Help)
      "Print this help message"
  ]

main :: IO ()
main = do
  argv <- getArgs
  case getOpt RequireOrder flags argv of
    (args, params, []) ->
      do
        if Help `elem` args
          then do
            hPutStrLn stderr (usageInfo usage flags)
            exitSuccess
          else do
            case params of
              [c, v, p] -> do
                let g = mkStdGen 42
                    dealtCards = dealCardsRandom (read c) (read v) (read p) g
                res <- evaluate $ force possibleSets dealtCards (read v)
                if Silent `elem` args
                  then exitSuccess
                  else
                    if Newline `elem` args
                      then mapM_ print res
                      else print res
              _ -> die $ usageInfo usage flags
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo usage flags)
      exitWith (ExitFailure 1)
  where
    usage = "Usage: paraset [-vn] <cards dealt> <number of values> <number of traits>"