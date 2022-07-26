{-# LANGUAGE UndecidableInstances #-}

module TheGame.Cards (CardStack, mkCardStack, partitionCards, splitExact) where

import Control.Monad.Error.Class (MonadError, throwError)
import Data.List (genericIndex)
import System.Random (Random, RandomGen, randomR)
import TheGame.Types (CardGameError (NotEnoughCardsForPlayers), Env (maxCard, minCard, players))

newtype CardStack a = MkCardStack {unCardStack :: Set a}
    deriving (IsList) via (Set a)

{- | from a given random generator create a cardstack with
   parameters as defined in the Env
-}
mkCardStack ::
    forall g m a.
    ( Integral a
    , Ord a
    , Random a
    , RandomGen g
    , MonadReader (Env a) m
    ) =>
    g ->
    m (CardStack a, g)
mkCardStack g = do
    env <- ask
    let minC :: a
        minC = minCard env

        maxC :: a
        maxC = maxCard env

        fac 0 = 1
        fac n = n * fac (n - 1)

        lst :: [[a]]
        lst = permutations [minC .. maxC]

        (i :: a, g' :: g) = randomR (0, fac (maxC - minC)) g
    pure (MkCardStack $ fromList $ lst `genericIndex` i, g')

-- | returns a cardstack for each of the players and a stack for drawing fresh cards
partitionCards ::
    forall m a.
    ( Integral a
    , MonadReader (Env a) m
    , MonadError CardGameError m
    ) =>
    CardStack a ->
    m ([CardStack a], CardStack a)
partitionCards cs = do
    env <- ask
    let go :: ([CardStack a], CardStack a) -> a -> m ([CardStack a], CardStack a)
        go tp 0 = pure tp
        go (st, r) n = do
            (c, r') <- splitExact n NotEnoughCardsForPlayers $ toList $ unCardStack r
            pure (fromList c : st, fromList r')
    go ([], cs) (players env)

-- | splits exactly
splitExact ::
    forall a b m e.
    ( Integral a
    , MonadError e m
    ) =>
    a ->
    e ->
    [b] ->
    m ([b], [b])
splitExact 0 _ xs = pure ([], xs)
splitExact _ e [] = throwError e
splitExact i e (x : xs) = first (x :) <$> splitExact (i - 1) e xs
