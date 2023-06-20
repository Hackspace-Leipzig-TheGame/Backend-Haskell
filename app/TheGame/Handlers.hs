module TheGame.Handlers (handleActs) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (TChan, TQueue, atomically, flushTQueue)
import Control.Monad (forever, (<=<))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.State (MonadState, evalStateT, gets, modify, runStateT)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (Identity))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as S
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import System.Random (getStdGen, setStdGen)
import System.Random.Stateful (runStateGen)
import TheGame.Cards (fisherYates, initCards)
import TheGame.Types
  ( Addressee (GameCast, SingleCast)
  , GameAction (CreateUser, JoinGame, Message, NewGame, StartGame, gameID, joinedGame, joinee, owner)
  , GameInstruction
  , GameResult
  , GameState (MkGameState, getGameState)
  , MessagePayload (FinishedGame, SpawnedGame)
  , Player (MkPlayer)
  , PlayerI
  , TheGame (MkTheGame, members, ownerID)
  , UserResponse (MkUserResponse, addressee, payload)
  )
import TheGame.Util (liftWriteTChan)

-- TODO leaving the game should be timed; i.e. if somebody has not responded on their turn after 10 seconds, they're timed out

handleActs :: TChan UserResponse -> TQueue (GameInstruction, PlayerI) -> IO a
handleActs globalRespChan globalActQueue = forever (runStateT go mempty)
 where
  go :: (MonadIO m, MonadState GameState m, MonadFail m) => m ()
  go = do
    acts <- liftIO $ atomically do flushTQueue globalActQueue
    traverse_ handleAct acts

  handleAct :: (MonadIO m, MonadState GameState m, MonadFail m) => (GameInstruction, PlayerI) -> m ()
  handleAct =
    traverse_ (liftWriteTChan globalRespChan) <=< \case
      (NewGame _ _, creator) -> createGame creator
      (JoinGame uuid _, player) -> joinGame uuid player
      (StartGame uuid, player) -> spawnGame uuid player globalRespChan
      (CreateUser, _) -> pure []
      (Message _, _) -> pure []

modifyGameState :: (MonadState GameState m) => (Map UUID TheGame -> Map UUID TheGame) -> m ()
modifyGameState f = modify (MkGameState . f . getGameState)

spawnGame :: (MonadIO m, MonadFail m, MonadState GameState m) => UUID -> PlayerI -> TChan UserResponse -> m [UserResponse]
spawnGame gameUUID player chan = do
  Just gs <- gets (fmap members . M.lookup gameUUID . getGameState)
  let mkResponse pl = MkUserResponse {addressee = GameCast gs, payload = pl}
  liftIO $ withAsync (playGame gameUUID player) \as -> do
    res <- wait as
    liftWriteTChan chan (mkResponse (Message (Identity (FinishedGame gameUUID res))))
  pure [mkResponse (Message (Identity (SpawnedGame gameUUID)))]

playGame :: UUID -> PlayerI -> IO GameResult
playGame gameUUID player = do
  gen <- getStdGen
  let (shuffled, gen') = runStateGen gen (fisherYates 100)
  setStdGen gen'
  flip evalStateT (initCards shuffled) do
    _

createGame :: (MonadIO m, MonadState GameState m) => PlayerI -> m [UserResponse]
createGame p@(MkPlayer (Identity pid) _pname) = do
  gameUUID <- liftIO UUID.nextRandom
  addGame gameUUID (MkTheGame {ownerID = pid, members = [p]})
  pure [MkUserResponse (SingleCast p) (NewGame {owner = Identity p, gameID = Identity gameUUID})]
 where
  addGame :: forall m'. (MonadState GameState m') => UUID -> TheGame -> m' ()
  addGame k v = modifyGameState (M.insert k v)

joinGame :: (MonadState GameState m, MonadFail m) => UUID -> PlayerI -> m [UserResponse]
joinGame gameUUID player = do
  let addUser g = g {members = player `S.insert` g.members}
  modifyGameState (M.adjust addUser gameUUID)
  Just gs <- gets (fmap members . M.lookup gameUUID . getGameState)
  pure [MkUserResponse {addressee = GameCast gs, payload = JoinGame {joinedGame = gameUUID, joinee = Identity player}}]
