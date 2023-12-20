module Main (main) where

import Control.DeepSeq
import Control.Exception
import Control.Monad
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), die, exitSuccess, exitWith)
import System.IO
import System.Random (StdGen, getStdGen, mkStdGen)
import V1 qualified
import V2 qualified
import V3 qualified
import V4 qualified
import V5 qualified
import V6Chunks qualified as V6C
import V6Naive qualified as V6
import V6Parbuffer qualified as V6P

type Card = [Int]

data Options = Options
  { optSilent :: Bool,
    optNewline :: Bool,
    optDeck :: Bool,
    optHelp :: Bool,
    optUsePresetSeed :: Bool,
    optRandSeed :: IO String,
    optVersion :: IO String
  }

startOptions :: Options
startOptions =
  Options
    { optSilent = False,
      optNewline = False,
      optDeck = False,
      optHelp = False,
      optUsePresetSeed = False,
      optRandSeed = return "42",
      optVersion = return "6P"
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
      ['v']
      ["version"]
      (ReqArg (\arg opt -> return opt {optVersion = return arg}) "6P")
      "Sets the version used (latest = 6). Valid options: 6P, 6C, 6, 5, 4, 3, 2, 1",
    Option
      []
      ["help"]
      (NoArg (\opt -> return opt {optHelp = True}))
      "Print this help message"
  ]

getVersionResults :: Int -> Int -> Int -> StdGen -> String -> ([[Card]], [Card])
getVersionResults c v p g version =
  case version of
    "6P" ->
      (force $ V6P.possibleSets dealtCards v p, map (V6P.generateCardFromIndex v p) dealtCards)
      where
        dealtCards = V6P.dealCardsRandom c v p g
    "6C" ->
      (force $ V6C.possibleSets dealtCards v p, map (V6C.generateCardFromIndex v p) dealtCards)
      where
        dealtCards = V6C.dealCardsRandom c v p g
    "6" ->
      (force $ V6.possibleSets dealtCards v p, map (V6.generateCardFromIndex v p) dealtCards)
      where
        dealtCards = V6.dealCardsRandom c v p g
    "5" ->
      (force $ V5.possibleSets dealtCards v p, map (V5.generateCardFromIndex v p) dealtCards)
      where
        dealtCards = V5.dealCardsRandom c v p g
    "4" ->
      (force $ V4.possibleSets dealtCards v p, map (V4.generateCardFromIndex v p) dealtCards)
      where
        dealtCards = V4.dealCardsRandom c v p g
    "3" ->
      (force $ V3.possibleSets dealtCards v, dealtCards)
      where
        dealtCards = V3.dealCardsRandom c v p g
    "2" ->
      (force $ V2.possibleSets dealtCards v, dealtCards)
      where
        dealtCards = V2.dealCardsRandom c v p g
    "1" ->
      (force $ V1.possibleSets dealtCards v, dealtCards)
      where
        dealtCards = V1.dealCardsRandom c v p g
    _ -> error "Invalid version"

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
                optRandSeed = seed,
                optVersion = version
              } = opts
        when help $ do
          hPutStrLn stderr (usageInfo usage options)
          exitSuccess
        case params of
          [c, v, p] -> do
            inputSeed <- seed
            ver <- version
            let presetG = mkStdGen $ read inputSeed
            g <- if presetSeed then return presetG else getStdGen
            let (res, dealtCardsAsCards) = getVersionResults (read c) (read v) (read p) g ver
            evalRes <- evaluate res
            when silent exitSuccess
            when deck $ do
              putStrLn "Dealt cards:"
              print dealtCardsAsCards
              putStrLn "Solutions:"
            if newline
              then mapM_ print evalRes
              else print res
          _ -> die $ usageInfo usage options
    (_, _, errs) -> do
      hPutStrLn stderr (concat errs ++ usageInfo usage options)
      exitWith (ExitFailure 1)
  where
    usage = "Usage: paraset [flags] <cards dealt> <number of values> <number of traits>"