module Main where

import           Control.DeepSeq       (deepseq)
import           Criterion.Main
import           Data.Foldable         (toList)
import qualified Data.Sequence         as S
import           System.Environment    (getArgs, withArgs)
import           System.Random         (getStdGen, mkStdGen)
import qualified System.Random.Shuffle as RS

import Random

-- Provide statistical benchmarks, but also "fixed amount" versions per method
-- for ease of profiling.
main = do
  args <- getArgs

  case args of
    ("--compare":xs) -> withArgs xs mainBench
    ["--fixed-rs"] -> mainRS
    ["--fixed-shuffle"] -> mainShuffle
    ["--fixed-shuffleseq"] -> mainShuffleSeq
    _ -> putStrLn "Valid args: --compare, --fixed-rs, --fixed-shuffle, --fixed-shuffleseq"

n :: Int
n = 100000

f1 xs rng = RS.shuffle' xs (length xs) rng
f2 xs rng = fst . shuffle rng $ xs
f3 xs rng = fst . shuffleSeq rng $ xs

mainRS         = runLoop f1 id
mainShuffle    = runLoop f2 id
mainShuffleSeq = runLoop f3 S.fromList

runLoop testf dataf = do
  let testData = dataf [0..n-1]

  results <- sequence (replicate 10 (f testData))
  putStrLn . show $ (map (\x -> x `deepseq` 1) results)

  where
    f testData = do
          rng <- getStdGen
          return $ testf testData rng

mainBench = do
  let testData = S.fromList [0..n-1]

  defaultMain [
    bgroup "shuffle"
      [ bench "System.Random.Shuffle" $ nfIO (f1 (toList testData) <$> getStdGen)
      , bench "Random.shuffle"        $ nfIO (f2 (toList testData) <$> getStdGen)
      , bench "Random.shuffleSeq"     $ nfIO (f3 testData          <$> getStdGen)
      ]
    ]
