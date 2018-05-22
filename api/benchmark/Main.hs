module Main where

import Random
import qualified System.Random.Shuffle as RS
import System.Random (mkStdGen)
import Criterion.Main
import qualified Data.Sequence as S
import Data.Foldable (toList)

main = do
  let rng = mkStdGen 0
  let n = 1000000
  let testData = S.fromList [0..n-1]
  let testSeq = testData
  let f1 xs = RS.shuffle' xs (length xs) rng
  let f2 = fst . shuffle rng
  let f3 = fst . shuffleSeq rng

  defaultMain [
    bgroup "shuffle"
      [ bench "System.Random.Shuffle"     $ whnf f1 (toList testData)
      , bench "our shuffle no conversion" $ whnf f3 testSeq
      , bench "our shuffle"               $ whnf f2 (toList testData)
      ]
    ]
