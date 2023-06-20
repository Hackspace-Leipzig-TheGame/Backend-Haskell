module TheGame.Types
  ( Player (..)
  , GameAction (..)
  , TheGameError (..)
  , GameState (..)
  , UserResponse (..)
  , TheGame (..)
  , Cards (..)
  , Addressee (..)
  )
where

import Control.Monad.Identity (Identity)
import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)

type Player :: (Type -> Type) -> Type
data Player f = MkPlayer
  { playerID :: f UUID
  , playerName :: Text
  }

-- >>> import Data.Aeson
-- >>> import Data.Functor.Const
-- >>> encode (NewGame (Const ()) (Const ()))
-- "{\"gameID\":[],\"owner\":[],\"tag\":\"NewGame\"}"

type GameAction :: (Type -> Type) -> Type
data GameAction f
  = NewGame {owner :: f (Player f), gameID :: f UUID}
  | JoinGame {joinedGame :: UUID, joinee :: f (Player Identity)}
  | CreateUser

type TheGameError :: Type
data TheGameError
  = InvalidJSONError
  | PlayerHandshakeFailed
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

type GameState :: Type
newtype GameState = MkGameState
  {getGameState :: Map UUID TheGame}
  deriving stock (Eq, Ord, Show, Generic)
  deriving newtype (Semigroup, Monoid)
  deriving anyclass (ToJSON)

data TheGame = MkTheGame
  { ownerID :: UUID
  , members :: Set (Player Identity)
  , cards :: Cards
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data Cards = MkCards
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- TODO rename in UserPayload f
type UserResponse :: Type
data UserResponse = MkUserResponse
  { addressee :: Addressee
  , payload :: GameAction Identity
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

data Addressee
  = SingleCast (Player Identity)
  | BroadCast
  | GameCast (Set (Player Identity))
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- Player

deriving stock instance (forall a. (Eq a) => Eq (f a)) => (Eq (Player f))

deriving stock instance (forall a. (Ord a) => Ord (f a), forall a. (Eq a) => Eq (f a)) => (Ord (Player f))

deriving stock instance (forall a. (Show a) => Show (f a)) => (Show (Player f))

deriving stock instance (forall a. (Generic a) => Generic (f a)) => (Generic (Player f))

deriving anyclass instance (forall a. (FromJSON a) => FromJSON (f a), forall a. (Generic a) => Generic (f a)) => (FromJSON (Player f))

deriving anyclass instance (forall a. (ToJSON a) => ToJSON (f a), forall a. (Generic a) => Generic (f a)) => (ToJSON (Player f))

-- GameAction

deriving stock instance (forall a. (Eq a) => Eq (f a)) => (Eq (GameAction f))

deriving stock instance (forall a. (Ord a) => Ord (f a), forall a. (Eq a) => Eq (f a)) => (Ord (GameAction f))

deriving stock instance (forall a. (Show a) => Show (f a)) => (Show (GameAction f))

deriving stock instance (forall a. (Generic a) => Generic (f a)) => (Generic (GameAction f))

deriving anyclass instance (forall a. (FromJSON a) => FromJSON (f a), forall a. (Generic a) => Generic (f a)) => (FromJSON (GameAction f))

deriving anyclass instance (forall a. (ToJSON a) => ToJSON (f a), forall a. (Generic a) => Generic (f a)) => (ToJSON (GameAction f))
