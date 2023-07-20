module TheGame.Handlers (handleActs) where

import Control.Concurrent.Async (wait, withAsync)
import Control.Concurrent.STM (STM, TChan, TQueue, TVar, atomically, flushTQueue, modifyTVar', newTVarIO, readTVar, registerDelay, retry, writeTQueue)
import Control.Monad (forever, (<=<))
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Identity (Identity (runIdentity))
import Control.Monad.State (MonadState (get, put), StateT, evalStateT, gets, modify, runState, runStateT)
import Data.Foldable (traverse_)
import Data.Functor.Identity (Identity (Identity))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as S
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import System.Random (getStdGen, setStdGen)
import System.Random.Stateful (runStateGen)
import TheGame.Cards (fisherYates, giveCards, initCards, playerRing)
import TheGame.Types
  ( Addressee (GameCast, SingleCast)
  , CardAction (GiveUp, PutCards)
  , CardStack
  , Cards (MkCards, drawStack)
  , GameAction (CreateUser, JoinGame, Message, NewGame, PlayGame, RemoveUser, StartGame, UpdatePlayer, gameID, joinedGame, joinee, owner)
  , GameInstruction
  , GameResult (Lost, Won)
  , GameState (MkGameState, getGameState)
  , MessagePayload (Acted, FinishedGame, SpawnedGame)
  , Player (MkPlayer, playerID)
  , PlayerI
  , RoundResult (PlayerPlayed, PlayerTimedOut)
  , TheGame (MkTheGame, gameActs, members, ownerID)
  , TheGameError (InvalidGameUUID)
  , UserResponse (MkUserResponse, addressee, payload)
  , playerCards
  , pattern ErrorMessage
  )
import TheGame.Util (liftWriteTChan)

-- TODO leaving the game should be timed; i.e. if somebody has not responded on their turn after 10 seconds, they're timed out

handleActs :: TChan UserResponse -> TQueue (GameInstruction, PlayerI) -> IO a
handleActs globalRespChan globalActQueue = forever (runStateT go mempty)
 where
  go = do
    acts <- liftIO $ atomically do flushTQueue globalActQueue
    traverse_ handleAct acts

  -- the game queue processing is done by function; this assumes that
  -- no player is malicious, which makes sense as otherwise the game
  -- doesn't make sense anyway (e.g. anyone could request to remove a
  -- player with a certain uuid at any time
  handleAct :: (GameInstruction, PlayerI) -> StateT GameState IO ()
  handleAct =
    -- FIXME: this design is flawed, we have to be very careful
    --        to not do anything that is not authorized
    --        we probably want to authorize the backend in a special way
    --        perhaps we want a special functor that tags the origin like \r -> (Bool, r)
    traverse_ (liftWriteTChan globalRespChan) <=< \case
      (NewGame _ _, creator) -> createGame creator
      (JoinGame uuid _, player) -> perUserError player $ joinGame uuid player
      (StartGame uuid, player) -> perUserError player $ spawnGame uuid player globalRespChan globalActQueue
      (PlayGame uuid act, player) -> perUserError player do
        game <- maybe (throwError (InvalidGameUUID uuid)) pure =<< gets (M.lookup uuid . getGameState)
        liftIO $ atomically do
          modifyTVar' game.gameActs (M.insert player (Just act))
        pure [MkUserResponse {addressee = SingleCast player, payload = Message (Acted act)}]
      (RemoveUser uuid, player) -> perUserError player do
        game <- maybe (throwError (InvalidGameUUID uuid)) pure =<< gets (M.lookup uuid . getGameState)
        modifyGameState (M.adjust (\g -> g {members = S.delete player g.members}) uuid)
        pure [MkUserResponse {addressee = GameCast game.members, payload = RemoveUser uuid}]
      -- FIXME: Should require something like authentification
      (UpdatePlayer uuid, player) -> perUserError player do
        game <- maybe (throwError (InvalidGameUUID uuid)) pure =<< gets (M.lookup uuid . getGameState)
        modifyGameState (M.adjust (\g -> g {members = S.insert player g.members}) uuid)
        pure [MkUserResponse {addressee = GameCast game.members, payload = RemoveUser uuid}]
      (CreateUser, _) -> pure []
      (Message _, _) -> pure []
   where
    perUserError :: (Functor m) => PlayerI -> ExceptT TheGameError m [UserResponse] -> m [UserResponse]
    perUserError player m = either (\err -> [MkUserResponse {addressee = SingleCast player, payload = ErrorMessage err}]) id <$> runExceptT m

modifyGameState :: (MonadState GameState m) => (Map UUID TheGame -> Map UUID TheGame) -> m ()
modifyGameState f = modify (MkGameState . f . getGameState)

spawnGame
  :: (MonadIO m, MonadState GameState m, MonadError TheGameError m)
  => UUID
  -> PlayerI
  -> TChan UserResponse
  -> TQueue (GameInstruction, PlayerI)
  -> m [UserResponse]
spawnGame gameUUID player respChan actQueue = do
  game <- maybe (throwError (InvalidGameUUID gameUUID)) pure =<< gets (M.lookup gameUUID . getGameState)
  if game.ownerID == runIdentity player.playerID
    then do
      let gs = members game
          mkResponse pl = MkUserResponse {addressee = GameCast gs, payload = pl}
      liftIO $ withAsync (playGame gameUUID game.gameActs actQueue gs) \as -> do
        res <- wait as
        liftWriteTChan respChan (mkResponse (Message (FinishedGame gameUUID res)))
      pure [mkResponse (Message (SpawnedGame gameUUID))]
    else pure [MkUserResponse {addressee = SingleCast player, payload = ErrorMessage (InvalidGameUUID gameUUID)}]

playGame :: UUID -> TVar (Map PlayerI (Maybe CardAction)) -> TQueue (GameInstruction, PlayerI) -> Set PlayerI -> IO GameResult
playGame gameUUID acts actQueue players' = do
  (shuffled, gen') <- flip runStateGen (fisherYates 100) <$> getStdGen
  setStdGen gen'
  let (players'', cards) = runState (giveCards (playerRing players')) (initCards shuffled)
  traverse_ (atomically . updatePlayer) players''
  evalStateT go (cards, players'')
 where
  updatePlayer :: PlayerI -> STM ()
  updatePlayer = writeTQueue actQueue . (UpdatePlayer gameUUID,)

  performMove :: [(CardStack, Int)] -> m (Either TheGameError PlayerI)
  performMove = _

  hasWon :: Cards -> Seq PlayerI -> Maybe GameResult
  hasWon (MkCards {drawStack = []}) ps | all (null . runIdentity . playerCards) ps = Just Won
  hasWon _ _ = Nothing

  go :: StateT (Cards, Seq PlayerI) IO GameResult
  go = do
    -- FIXME: how do we keep the player state up to date? perhaps send player state updates via chan?
    (cards, p Seq.:<| ps) <- get -- if this fails it can be an IO error; sadly there's no NonEmpty Seq
    timeOut <- liftIO $ registerDelay 30_000_000
    roundResult <- liftIO $ atomically do
      currentMap <- readTVar acts
      case M.lookup p currentMap of
        Just (Just act) -> pure $ PlayerPlayed act
        _ -> readTVar timeOut >>= \b -> if b then pure PlayerTimedOut else retry
    liftIO $ atomically do
      modifyTVar' acts (M.insert p Nothing)
    case roundResult of
      PlayerTimedOut -> do
        put (cards, ps)
        liftIO (atomically do writeTQueue actQueue (RemoveUser gameUUID, p))
        -- we remove the player from the Set, they cannot play anymore
        go
      PlayerPlayed act -> case act of
        GiveUp -> pure Lost
        PutCards move ->
          performMove move >>= \case
            Right player -> do
              liftIO $ atomically do
                writeTQueue actQueue (UpdatePlayer gameUUID, player)
              get >>= maybe go pure . uncurry hasWon
            Left err -> do
              put (cards, ps)
              liftIO $ atomically do
                writeTQueue actQueue (RemoveUser gameUUID, p)
                writeTQueue actQueue (ErrorMessage err, p)
              go

createGame :: (MonadIO m, MonadState GameState m) => PlayerI -> m [UserResponse]
createGame p@(MkPlayer (Identity pid) _pname _cards) = do
  gameUUID <- liftIO UUID.nextRandom
  acts <- liftIO $ newTVarIO mempty
  addGame gameUUID (MkTheGame {ownerID = pid, members = [p], gameActs = acts})
  pure [MkUserResponse (SingleCast p) (NewGame {owner = Identity p, gameID = Identity gameUUID})]
 where
  addGame :: forall m'. (MonadState GameState m') => UUID -> TheGame -> m' ()
  addGame k v = modifyGameState (M.insert k v)

joinGame :: (MonadState GameState m, MonadError TheGameError m) => UUID -> PlayerI -> m [UserResponse]
joinGame gameUUID player = do
  let addUser g = g {members = player `S.insert` g.members}
  modifyGameState (M.adjust addUser gameUUID)
  gs <- maybe (throwError (InvalidGameUUID gameUUID)) pure =<< gets (fmap members . M.lookup gameUUID . getGameState)
  pure [MkUserResponse {addressee = GameCast gs, payload = JoinGame {joinedGame = gameUUID, joinee = Identity player}}]
