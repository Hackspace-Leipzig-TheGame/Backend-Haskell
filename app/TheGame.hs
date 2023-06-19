module TheGame where

import Control.Concurrent.STM (TChan, TQueue, atomically, dupTChan, readTChan, writeTQueue)
import Control.Monad (forever, when)
import Data.Aeson (ToJSON, decode, encode)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity (Identity, runIdentity))
import Data.UUID.V4 qualified as UUID
import Network.WebSockets (Connection, ServerApp, acceptRequest, receiveData, sendTextData)
import TheGame.Types (GameAction (CreateUser), Player (MkPlayer, playerID, playerName), TheGameError (InvalidJSONError, PlayerHandshakeFailed), UserResponse (payload, playerID))

respondJSON :: (ToJSON a) => Connection -> a -> IO ()
respondJSON con = sendTextData con . encode

conLoop :: TChan UserResponse -> TQueue (GameAction (Const ())) -> Player Identity -> Connection -> IO ()
conLoop respChan actChan user con = do
  mact :: Maybe (GameAction (Const ())) <- decode <$> receiveData con
  case mact of
    Nothing -> respondJSON con InvalidJSONError
    Just act -> do
      atomically do writeTQueue actChan act
      resp <- atomically do readTChan respChan
      when (runIdentity (user.playerID) == resp.playerID) do
        respondJSON con resp.payload

handShakePlayer :: TQueue (GameAction (Const ())) -> Connection -> IO (Maybe (Player Identity))
handShakePlayer actChan con = do
  mplayerName :: Maybe (Player (Const ())) <- decode <$> receiveData con
  uuid <- UUID.nextRandom
  let mplayerWithID = (\player -> MkPlayer (Identity uuid) (player.playerName)) <$> mplayerName
  case mplayerWithID of
    Nothing -> respondJSON con PlayerHandshakeFailed
    Just playerWithID -> atomically do writeTQueue actChan (CreateUser playerWithID)
  pure mplayerWithID

theGameWebSocket :: TChan UserResponse -> TQueue (GameAction (Const ())) -> ServerApp
theGameWebSocket globalRespChan actChan pending = do
  con <- acceptRequest pending
  respChan <- atomically $ dupTChan globalRespChan
  Just player <- handShakePlayer actChan con
  forever do conLoop respChan actChan player con