module TheGame.Cards (fisherYates, initCards) where

import Control.Monad (foldM)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import System.Random.Stateful (StatefulGen, uniformRM)
import TheGame.Types
  ( Cards
      ( MkCards
      , drawStack
      , leftHundred
      , leftOne
      , rightHundred
      , rightOne
      )
  )

-- | implements an extremely stupid and slow version of a fisher yates shuffle
--
-- >>> import System.Random.Stateful
-- >>> gen = mkStdGen 42
-- >>> runStateGen_ gen (fisherYates 5)
-- [4,3,2,5,1]
fisherYates :: forall m g. (StatefulGen g m) => Int -> g -> m [Int]
fisherYates n gen =
  let sq :: Seq Int = [1 .. n]
      ixs :: [Int] = [n, n - 1 .. 1]
      go :: ([Int], Seq Int) -> Int -> m ([Int], Seq Int)
      go (acc, rm) i = do
        rnd <- uniformRM (0, i - 1) gen
        pure ((rm `Seq.index` rnd) : acc, Seq.deleteAt rnd rm)
   in fst <$> foldM go ([], sq) ixs

initCards :: [Int] -> Cards
initCards shuffled =
  MkCards
    { leftHundred = []
    , rightHundred = []
    , leftOne = []
    , rightOne = []
    , drawStack = shuffled
    }
