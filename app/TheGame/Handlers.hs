module TheGame.Handlers (handleActs) where

import Control.Concurrent.STM (TChan, TQueue, atomically, flushTQueue)
import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, runStateT)
import Data.Foldable (traverse_)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Data.UUID (UUID)
import TheGame.Types (GameAction (CreateUser, JoinGame, NewGame), GameState, Player, UserResponse)
import TheGame.Util (liftWriteTChan)

handleActs :: TChan UserResponse -> TQueue (GameAction (Const ()), Player Identity) -> IO a
handleActs globalRespChan globalActQueue = forever (runStateT go mempty)
 where
  go :: (MonadIO m, MonadState GameState m) => m ()
  go = do
    acts <- liftIO $ atomically do flushTQueue globalActQueue
    traverse_ handleAct acts

  handleAct :: (MonadIO m, MonadState GameState m) => (GameAction (Const ()), Player Identity) -> m ()
  handleAct =
    liftWriteTChan globalRespChan <=< \case
      (NewGame _ _, creator) -> createGame creator
      (JoinGame uuid, player) -> joinGame uuid player
      (CreateUser, player) -> registerPlayer player

createGame :: Player Identity -> m UserResponse
createGame = _

joinGame :: UUID -> Player Identity -> m UserResponse
joinGame = _

registerPlayer :: Player Identity -> m UserResponse
registerPlayer = _
