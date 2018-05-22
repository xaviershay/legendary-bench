{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Fast, reversible perfect shuffling.
module Random
  ( shuffle
  , shuffleSeq
  , unshuffle
  ) where

import           Data.Foldable (toList)
import qualified Data.Sequence as S
import           System.Random (RandomGen, mkStdGen, randomR)

-- /O(nlog(n))/ Randomize the order of a list with a "perfect shuffle": each
-- possible permuation is equally likely.
-- See http://okmij.org/ftp/Haskell/perfect-shuffle.txt for a description of
-- the algorithm. This implementation improves on it by using Data.Sequence
-- (which uses 2-3 finger trees) rather than a binary tree. As a result, it's
-- slightly faster.
shuffle :: RandomGen gen => gen -> [a] -> ([a], gen)
shuffle rng xs =
  let (sequence, rng') = rseq (length xs) rng in

  (f (S.fromList xs) sequence, rng')

  where

    f :: S.Seq a -> [Int] -> [a]
    f accum [] = mempty
    f accum (y:ys) =
      let accum' = S.deleteAt y accum in

      (accum `S.index` y) : f accum' ys

-- `shuffle` but for sequences. Is around 2-3x slower due to conversion
-- overhead.
shuffleSeq :: RandomGen gen => gen -> S.Seq a -> (S.Seq a, gen)
shuffleSeq gen xs =
  -- Surprisingly, converting to/from a list here was slightly faster than
  -- modifying the shuffle implementation to be "sequence native". Guess: less
  -- allocation burden due to reduced number of total sequence operations.
  let (xs', gen') = shuffle gen (toList xs) in

  (S.fromList xs', gen')

-- /O(nlog(n))/ Given a shuffled list and the RandomGen used to shuffle it,
-- return the unshuffled list.
unshuffle :: RandomGen gen => gen -> [a] -> ([a], gen)
unshuffle rng xs =
  let (sequence, rng') = rseq (length xs) rng in
  let unshuffled = toList . f mempty . reverse $
                     zip xs sequence in

  (unshuffled, rng')

  where
    f :: S.Seq a -> [(a, Int)] -> S.Seq a
    f accum [] = accum
    f accum ((x, i):xs) = f (S.insertAt i x accum) xs

-- This implementation based on Data.Random.Shuffle, changed to return the
-- generator. n must be greater than 1.
rseq :: RandomGen gen => Int -> gen -> ([Int], gen)
rseq 0 gen = ([], gen)
rseq n gen = (\(as, bs) -> (as, last bs)) . unzip . rseq' (n - 1) $ gen
  where
    rseq' :: RandomGen gen => Int -> gen -> [(Int, gen)]
    rseq' 0 gen = [(0, gen)]
    rseq' i gen = (j, gen) : rseq' (i - 1) gen'
      where
        (j, gen') = randomR (0, i) gen
