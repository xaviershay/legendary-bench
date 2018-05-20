module RandomTest where

import Random

import System.Random (mkStdGen)

prop_reversibleShuffle :: Int -> [Int] -> Bool
prop_reversibleShuffle seed xs =
  let rng = mkStdGen seed in

  (fst $ unshuffle rng (fst $ shuffle rng xs)) == (xs :: [Int])
