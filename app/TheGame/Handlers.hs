module TheGame.Handlers (handleActs) where

import Control.Concurrent.STM (TChan, TQueue, atomically, flushTQueue)
import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, gets, modify, runStateT)
import Data.Foldable (traverse_)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity (Identity))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import TheGame.Cards (initCards)
import TheGame.Types
  ( Addressee (GameCast, SingleCast)
  , GameAction (CreateUser, JoinGame, NewGame, gameID, joinedGame, joinee, owner)
  , GameState (MkGameState, getGameState)
  , Player (MkPlayer)
  , TheGame (MkTheGame, cards, members, ownerID)
  , UserResponse (MkUserResponse, addressee, payload)
  )
import TheGame.Util (liftWriteTChan)

-- TODO leaving the game should be timed; i.e. if somebody has not responded on their turn after 10 seconds, they're timed out

handleActs :: TChan UserResponse -> TQueue (GameAction (Const ()), Player Identity) -> IO a
handleActs globalRespChan globalActQueue = forever (runStateT go mempty)
 where
  go :: (MonadIO m, MonadState GameState m, MonadFail m) => m ()
  go = do
    acts <- liftIO $ atomically do flushTQueue globalActQueue
    traverse_ handleAct acts

  handleAct :: (MonadIO m, MonadState GameState m, MonadFail m) => (GameAction (Const ()), Player Identity) -> m ()
  handleAct =
    traverse_ (liftWriteTChan globalRespChan) <=< \case
      (NewGame _ _, creator) -> createGame creator
      (JoinGame uuid _, player) -> joinGame uuid player
      (CreateUser, _) -> pure []

modifyGameState :: (MonadState GameState m) => (Map UUID TheGame -> Map UUID TheGame) -> m ()
modifyGameState f = modify (MkGameState . f . getGameState)

createGame :: (MonadIO m, MonadState GameState m) => Player Identity -> m [UserResponse]
createGame p@(MkPlayer (Identity pid) _pname) = do
  gameUUID <- liftIO UUID.nextRandom
  addGame gameUUID (MkTheGame {ownerID = pid, cards = initCards, members = [p]})
  pure [MkUserResponse (SingleCast p) (NewGame {owner = Identity p, gameID = Identity gameUUID})]
 where
  addGame :: forall m'. (MonadState GameState m') => UUID -> TheGame -> m' ()
  addGame k v = modifyGameState (M.insert k v)

joinGame :: (MonadState GameState m, MonadFail m) => UUID -> Player Identity -> m [UserResponse]
joinGame gameUUID player = do
  let addUser g = g {members = player `S.insert` g.members}
  modifyGameState (M.adjust addUser gameUUID)
  Just gs <- gets (fmap members . M.lookup gameUUID . getGameState)
  pure [MkUserResponse {addressee = GameCast gs, payload = JoinGame {joinedGame = gameUUID, joinee = Identity player}}]
