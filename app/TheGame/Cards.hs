module TheGame.Cards (fisherYates, initCards, playerRing, nextPlayer, giveCards) where

import Control.Monad (foldM)
import Control.Monad.State (MonadState (get, put), gets, modify')
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
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
  , Player
  , playerCards
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
        pure (rm `Seq.index` rnd : acc, Seq.deleteAt rnd rm)
   in fst <$> foldM go ([], sq) ixs

playerRing :: Set a -> Seq a
playerRing = foldr (Seq.<|) []

-- | get the next player, only works on non-empty 'Seq's
nextPlayer :: (MonadState (Seq a) m) => m a
nextPlayer = do
  ps' <- get
  case ps' of
    (p Seq.:<| ps) -> do
      put (ps Seq.:|> p)
      pure p
    Seq.Empty -> error "nextPlayer: tried to get the next player from an empty Seq"

-- | inits cards with a shuffled stacks
initCards :: [Int] -> Cards
initCards shuffled =
  MkCards
    { leftHundred = []
    , rightHundred = []
    , leftOne = []
    , rightOne = []
    , drawStack = shuffled
    }

-- | gives cards to players
--
-- >>> import TheGame.Types
-- >>> import Control.Monad.State.Lazy
-- >>> p = MkPlayer Nothing (pure [])
-- >>> runStateT (giveCards  [ p "klaus", p "fritz"]) (initCards [1..16])
-- (fromList [MkPlayer {playerID = Nothing, playerCards = Just [1,2,3,4,5,6,7], playerName = "klaus"},MkPlayer {playerID = Nothing, playerCards = Just [8,9,10,11,12,13,14], playerName = "fritz"}],MkCards {leftOne = [], leftHundred = [], rightOne = [], rightHundred = [], drawStack = [15,16]})
giveCards :: forall m f. (MonadState Cards m, Applicative f) => Seq (Player f) -> m (Seq (Player f))
giveCards ps
  | length ps == 1 = give 8
  | length ps == 2 = give 7
  | otherwise = give 6
 where
  give :: Int -> m (Seq (Player f))
  give n =
    let go :: Seq (Player f) -> Player f -> m (Seq (Player f))
        go acc p = do
          (cs, remaining) <- gets (splitAt n . drawStack)
          modify' \cs' -> cs' {drawStack = remaining}
          pure (acc Seq.|> p {playerCards = pure cs})
     in foldM go [] ps
