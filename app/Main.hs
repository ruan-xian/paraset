module Main (main) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import Lib (dealCardsRandom, possibleSets)
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), die, exitSuccess, exitWith)
import System.IO
import System.Random (getStdGen, mkStdGen)

data Flag
  = Silent -- -s
  | Newline -- -n
  | Deck -- -d
  | Help -- --help
  | Randseed String -- -r
  deriving (Eq, Show)

data Options = Options
  { optSilent :: Bool,
    optNewline :: Bool,
    optDeck :: Bool,
    optHelp :: Bool,
    optUsePresetSeed :: Bool,
    optRandSeed :: IO String
  }

startOptions :: Options
startOptions =
  Options
    { optSilent = False,
      optNewline = False,
      optDeck = False,
      optHelp = False,
      optUsePresetSeed = False,
      optRandSeed = return "42"
    }

options :: [OptDescr (Options -> IO Options)]
options =
  [ Option
      ['s']
      ["silent"]
      (NoArg (\opt -> return opt {optSilent = True}))
      "Silences all output.",
    Option
      ['d']
      ["deck"]
      (NoArg (\opt -> return opt {optDeck = True}))
      "Prints the deck generated.",
    Option
      ['n']
      ["newline"]
      (NoArg (\opt -> return opt {optNewline = True}))
      "Prints each solution on a separate line.",
    Option
      ['r']
      ["randseed"]
      (ReqArg (\arg opt -> return opt {optUsePresetSeed = True, optRandSeed = return arg}) "42")
      "Sets the random seed used.",
    Option
      []
      ["help"]
      (NoArg (\opt -> return opt {optHelp = True}))
      "Print this help message"
  ]

main :: IO ()
main = do
  argv <- getArgs
  case getOpt RequireOrder options argv of
    (actions, params, []) ->
      do
        opts <- foldl (>>=) (return startOptions) actions
        let Options
              { optSilent = silent,
                optNewline = newline,
                optDeck = deck,
                optHelp = help,
                optUsePresetSeed = presetSeed,
                optRandSeed = seed
              } = opts
        when help $ do
          hPutStrLn stderr (usageInfo usage options)
          exitSuccess
        case params of
          [c, v, p] -> do
            inputSeed <- seed
            let presetG = mkStdGen $ read inputSeed
            g <- if presetSeed then return presetG else getStdGen
            let dealtCards = dealCardsRandom (read c) (read v) (read p) g
            res <- evaluate $ force possibleSets dealtCards (read v)
            when silent exitSuccess
            when deck $ do
              putStrLn "Dealt cards:"
              print dealtCards
              putStrLn "Solutions:"
            if newline
              then mapM_ print res
              else print res
          _ -> die $ usageInfo usage options
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo usage options)
      exitWith (ExitFailure 1)
  where
    usage = "Usage: paraset [-vn] <cards dealt> <number of values> <number of traits>"