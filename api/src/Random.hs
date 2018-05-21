-- Fast, reversible perfect shuffling.
--
-- On my benchmarks this implementation was about 100x faster than
-- System.Random.Shuffle
module Random
  ( shuffle
  , unshuffle
  ) where

import           Data.Foldable (toList)
import qualified Data.Sequence as S
import           System.Random (RandomGen, mkStdGen, randomR)

-- /O(nlog(n))/ Randomize the order of a list with a "perfect shuffle": each
-- possible permuation is equally likely.
-- See http://okmij.org/ftp/Haskell/perfect-shuffle.txt for a description of
-- the algorithm. This implementation improves on it by using Data.Sequence
-- (which uses 2-3 finger trees) rather than a binary tree.
shuffle :: RandomGen gen => gen -> [a] -> ([a], gen)
shuffle rng [] = ([], rng)
shuffle rng xs =
  let (sequence, rng') = rseq (length xs) rng in

  (f (S.fromList xs) sequence, rng')

  where

    f :: S.Seq a -> [Int] -> [a]
    f accum [] = [accum `S.index` 0]
    f accum (y:ys) =
      let accum' = S.deleteAt y accum in

      accum `S.index` y : f accum' ys

-- /O(nlog(n))/ Given a shuffled list and the RandomGen used to shuffle it,
-- return the unshuffled list.
unshuffle :: RandomGen gen => gen -> [a] -> ([a], gen)
unshuffle rng [] = ([], rng)
unshuffle rng xs =
  let (sequence, rng') = rseq (length xs) rng in
    (toList . f mempty . reverse $ zip xs (sequence ++ [0]), rng')
  where
    f :: S.Seq a -> [(a, Int)] -> S.Seq a
    f accum [] = accum
    f accum ((x, i):xs) = f (S.insertAt i x accum) xs

-- This implementation based on Data.Random.Shuffle, changed to return the
-- generator. n must be greater than 1.
rseq :: RandomGen gen => Int -> gen -> ([Int], gen)
rseq n = (\(as, bs) -> (as, last bs)) . unzip . rseq' (n - 1)
  where
    rseq' :: RandomGen gen => Int -> gen -> [(Int, gen)]
    rseq' 0 _ = []
    rseq' i gen = (j, gen) : rseq' (i - 1) gen'
      where
        (j, gen') = randomR (0, i) gen
