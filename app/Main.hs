module Main (main) where

import Control.DeepSeq (force)
import Control.Exception (IOException, catch, evaluate)
import Control.Monad (when)
import Data.List (sort)
import ParsetBase qualified
import System.Console.GetOpt
  ( ArgDescr (NoArg, ReqArg),
    ArgOrder (RequireOrder),
    OptDescr (..),
    getOpt,
    usageInfo,
  )
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), die, exitSuccess, exitWith)
import System.IO (hPutStrLn, stderr)
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
    optVersion :: IO String,
    optDealtCardsFile :: Maybe String
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
      optVersion = return "6P",
      optDealtCardsFile = Nothing
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
      ['f']
      ["file"]
      (ReqArg (\arg opt -> return opt {optDealtCardsFile = Just arg}) "")
      "Optional filename containing dealt cards; otherwise randomly generated",
    Option
      ['r']
      ["randseed"]
      (ReqArg (\arg opt -> return opt {optUsePresetSeed = True, optRandSeed = return arg}) "42")
      "Sets the random seed used.",
    Option
      ['v']
      ["version"]
      (ReqArg (\arg opt -> return opt {optVersion = return arg}) "6P")
      "Sets the version used (latest = 6P). Valid options: 6P, 6C, 6, 5, 4, 3, 2, 1",
    Option
      []
      ["help"]
      (NoArg (\opt -> return opt {optHelp = True}))
      "Print this help message"
  ]

getVersionResults :: Int -> Int -> Int -> StdGen -> [Card] -> String -> ([[Card]], [Card])
getVersionResults c v p g dealtCards version =
  case version of
    "6P" ->
      (force $ V6P.possibleSets dealtCards v, dealtCards)
    "6C" ->
      (force $ V6C.possibleSets dealtCards v, dealtCards)
    "6" ->
      (force $ V6.possibleSets dealtCards v, dealtCards)
    "5" ->
      (force $ V5.possibleSets dealtCards v, dealtCards)
    "4" ->
      (force $ V4.possibleSets dealtCardsV4 v p, map (V4.generateCardFromIndex v p) dealtCardsV4)
      where
        dealtCardsV4 = V4.dealCardsRandom c v p g
    "3" ->
      (force $ V3.possibleSets dealtCards v, dealtCards)
    "2" ->
      (force $ V2.possibleSets dealtCards v, dealtCards)
    "1" ->
      (force $ V1.possibleSets dealtCards v, dealtCards)
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

            dealtCards <- case optDealtCardsFile opts of
              Just filename -> do
                contents <- catch (readFile filename) handleReadFileError
                let parsedContents = reads contents :: [([Card], String)]
                if null parsedContents
                  then handleParseError
                  else return $ sort $ fst $ head parsedContents
              Nothing -> return $ ParsetBase.dealCardsRandom (read c) (read v) (read p) g

            let (res, dealtCardsAsCards) = getVersionResults (read c) (read v) (read p) g dealtCards ver
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

handleReadFileError :: IOException -> IO String
handleReadFileError _ = do
  putStrLn "Error: Could not read the file."
  exitWith (ExitFailure 1)

handleParseError :: IO [Card]
handleParseError = do
  putStrLn "Error: File contents cannot be parsed as Cards. See test/sampleDeal.txt for an example."
  exitWith (ExitFailure 1)