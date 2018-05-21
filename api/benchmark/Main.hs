module Main where

import Random
import qualified System.Random.Shuffle as RS
import System.Random (mkStdGen)
import Criterion.Main

main = do
  let rng = mkStdGen 0
  let n = 1000000
  let testData = [0..n-1]
  let f1 xs = RS.shuffle' xs (length xs) rng
  let f2 = fst . shuffle rng

  defaultMain [
    bgroup "shuffle"
      [ bench "System.Random.Shuffle" $ whnf f1 testData
      , bench "our shuffle"           $ whnf f2 testData
      ]
    ]
