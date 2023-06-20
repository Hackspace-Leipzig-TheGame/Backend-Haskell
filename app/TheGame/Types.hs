module TheGame.Types
  ( Player (..)
  , PlayerI
  , GameAction (..)
  , GameInstruction
  , TheGameError (..)
  , GameState (..)
  , UserResponse (..)
  , TheGame (..)
  , Cards (..)
  , Addressee (..)
  , MessagePayload (..)
  , GameResult (..)
  )
where

import Control.Applicative (Const)
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

type PlayerI :: Type
type PlayerI = Player Identity

-- >>> import Data.Aeson
-- >>> import Data.Functor.Const
-- >>> encode (NewGame (Const ()) (Const ()))
-- "{\"gameID\":[],\"owner\":[],\"tag\":\"NewGame\"}"

type GameAction :: (Type -> Type) -> Type
data GameAction f
  = NewGame {owner :: f (Player f), gameID :: f UUID}
  | JoinGame {joinedGame :: UUID, joinee :: f (Player Identity)}
  | CreateUser
  | StartGame {startedGameDas :: UUID}
  | Message {payload :: f MessagePayload}

type GameInstruction :: Type
type GameInstruction = GameAction (Const ())

type MessagePayload :: Type
data MessagePayload
  = SpawnedGame {startedGame :: UUID}
  | FinishedGame {finishedGame :: UUID, gameResult :: GameResult}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type GameResult :: Type
data GameResult = MkGameResult
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

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

type TheGame :: Type
data TheGame = MkTheGame
  { ownerID :: UUID
  , members :: Set (Player Identity)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON)

type Cards :: Type
data Cards = MkCards
  { leftOne :: [Int]
  , leftHundred :: [Int]
  , rightOne :: [Int]
  , rightHundred :: [Int]
  , drawStack :: [Int]
  }
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

type Addressee :: Type
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
