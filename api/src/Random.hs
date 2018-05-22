{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- Fast, reversible perfect shuffling.
--
-- On my benchmarks this implementation was about 100x faster than
-- System.Random.Shuffle
module Random
  ( shuffle
  , unshuffle
  , shuffleSeq
  ) where

import           Data.Foldable (toList)
import qualified Data.Sequence as S
import           System.Random (RandomGen, mkStdGen, randomR)

-- A somewhat excessive typeclass to enable functions to be made generic across
-- different list like classes.
class SeqLike m a where
  toSeq :: m a -> S.Seq a
  fromSeq :: S.Seq a -> m a
  cons :: a -> m a -> m a

instance SeqLike ([]) a where
  toSeq = S.fromList
  fromSeq = toList
  cons = (:)

instance SeqLike S.Seq a where
  toSeq = id
  fromSeq = id
  cons = (S.<|)

-- /O(nlog(n))/ Randomize the order of a list with a "perfect shuffle": each
-- possible permuation is equally likely.
-- See http://okmij.org/ftp/Haskell/perfect-shuffle.txt for a description of
-- the algorithm. This implementation improves on it by using Data.Sequence
-- (which uses 2-3 finger trees) rather than a binary tree.
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

-- This implementation has radically slower performance than `shuffle`. I don't
-- know why.
shuffleSeq :: RandomGen gen => gen -> S.Seq a -> (S.Seq a, gen)
shuffleSeq rng xs =
  let xs' = xs in
  let (sequence, rng') = rseq (S.length xs') rng in

  (f xs' sequence, rng')

  where

    f :: S.Seq a -> [Int] -> S.Seq a
    f accum [] = mempty
    f accum (y:ys) =
      let accum' = S.deleteAt y accum in

      (accum `S.index` y) S.<| f accum' ys

-- /O(nlog(n))/ Given a shuffled list and the RandomGen used to shuffle it,
-- return the unshuffled list.
unshuffle :: (SeqLike m a, RandomGen gen) => gen -> m a -> (m a, gen)
unshuffle rng xs =
  let xs' = toSeq xs in
  let (sequence, rng') = rseq (S.length xs') rng in
  let unshuffled = fromSeq . f mempty . S.reverse $
                     S.zip xs' (S.fromList sequence S.|> 0) in

  (unshuffled, rng')

  where
    f :: S.Seq a -> S.Seq (a, Int) -> S.Seq a
    f accum S.Empty = accum
    f accum ((x, i) S.:<| xs) = f (S.insertAt i x accum) xs

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
