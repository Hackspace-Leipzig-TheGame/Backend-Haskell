module TheGame.Types
  ( Player (..)
  , PlayerI
  , GameAction (..)
  , GameInstruction
  , TheGameError (..)
  , MoveError (..)
  , GameState (..)
  , UserResponse (..)
  , TheGame (..)
  , Cards (..)
  , Addressee (..)
  , MessagePayload (..)
  , GameResult (..)
  , CardAction (..)
  , CardStack (..)
  , RoundResult (..)
  , pattern ErrorMessage
  )
where

import Control.Applicative (Const)
import Control.Concurrent.STM (TVar)
import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Identity (Identity (..))
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
  , playerCards :: f [Int]
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
  | StartGame {startedGame :: UUID}
  | PlayGame {playedGame :: UUID, theAction :: CardAction}
  | UpdatePlayer {playedGame :: UUID}
  | Message {payload :: MessagePayload}
  | CreateUser
  | RemoveUser {removedGame :: UUID}

data CardAction
  = GiveUp
  | PutCards [(CardStack, Int)]
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CardStack = LeftOne | RightOne | LeftHundred | RightHundred
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

type GameInstruction :: Type
type GameInstruction = GameAction (Const ())

pattern ErrorMessage :: TheGameError -> GameAction f
pattern ErrorMessage msg = Message (Error msg)

type MessagePayload :: Type
data MessagePayload
  = SpawnedGame {startedGame :: UUID}
  | Acted CardAction
  | FinishedGame {finishedGame :: UUID, gameResult :: GameResult}
  | Error TheGameError
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type GameResult :: Type
data GameResult
  = Lost
  | Won
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type TheGameError :: Type
data TheGameError
  = InvalidJSONError
  | PlayerHandshakeFailed
  | InvalidGameUUID UUID
  | GameNotStarted UUID
  | InvalidMove MoveError
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MoveError
  = NotEnoughCards
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type GameState :: Type
newtype GameState = MkGameState
  {getGameState :: Map UUID TheGame}
  deriving stock (Eq, Generic)
  deriving newtype (Semigroup, Monoid)

type RoundResult :: Type
data RoundResult
  = PlayerTimedOut
  | PlayerPlayed CardAction
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

type TheGame :: Type
data TheGame = MkTheGame
  { ownerID :: UUID
  , gameActs :: TVar (Map PlayerI (Maybe CardAction))
  , members :: Set (Player Identity)
  }
  deriving stock (Eq, Generic)

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
