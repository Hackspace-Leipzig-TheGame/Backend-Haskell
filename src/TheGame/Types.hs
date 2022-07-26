module TheGame.Types (Err, Env (..), CardGameError (..), defaultEnv) where

type Err a = Either Text a

data Env a = MkEnv
    { -- | the lowest card
      minCard :: a
    , -- | the highest card
      maxCard :: a
    , -- | the number of cards that each player gets
      cardsPerPlayer :: a
    , -- | the number of players per game
      players :: a
    }

defaultEnv :: Integral a => Env a
defaultEnv =
    MkEnv
        { minCard = 2
        , maxCard = 99
        , cardsPerPlayer = 6
        , players = 4
        }

data CardGameError
    = -- | in the configuration there are not enough cards for all the players
      NotEnoughCardsForPlayers
