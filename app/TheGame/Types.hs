module TheGame.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
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
  | JoinGame {gameID :: f UUID}
  | CreateUser {playerID :: Player Identity}

type TheGameError :: Type
data TheGameError
  = InvalidJSONError
  | PlayerHandshakeFailed
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

type GameState :: Type
data GameState
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

type UserResponse :: Type
data UserResponse = MkUserResponse
  { playerID :: UUID
  , payload :: UserResponsePayload
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

type UserResponsePayload :: Type
data UserResponsePayload = MkUserResponsePayload
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

-- Player

deriving stock instance (forall a. (Eq a) => Eq (f a)) => (Eq (Player f))

deriving stock instance (forall a. (Show a) => Show (f a)) => (Show (Player f))

deriving stock instance (forall a. (Generic a) => Generic (f a)) => (Generic (Player f))

deriving anyclass instance (forall a. (FromJSON a) => FromJSON (f a), forall a. Generic a => Generic (f a)) => (FromJSON (Player f))

deriving anyclass instance (forall a. (ToJSON a) => ToJSON (f a), forall a. Generic a => Generic (f a)) => (ToJSON (Player f))

-- GameAction

deriving stock instance (forall a. (Eq a) => Eq (f a)) => (Eq (GameAction f))

deriving stock instance (forall a. (Show a) => Show (f a)) => (Show (GameAction f))

deriving stock instance (forall a. (Generic a) => Generic (f a)) => (Generic (GameAction f))

deriving anyclass instance (forall a. (FromJSON a) => FromJSON (f a), forall a. Generic a => Generic (f a)) => (FromJSON (GameAction f))

deriving anyclass instance (forall a. (ToJSON a) => ToJSON (f a), forall a. Generic a => Generic (f a)) => (ToJSON (GameAction f))
