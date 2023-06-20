module TheGame (theGameWebSocket) where

import Control.Concurrent.STM (TChan, TQueue, atomically, dupTChan, readTChan, writeTQueue)
import Control.Monad (forever, when)
import Data.Aeson (decode)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity (Identity))
import Data.UUID.V4 qualified as UUID
import Network.WebSockets (Connection, ServerApp, acceptRequest, receiveData)
import TheGame.Types
  ( Addressee (BroadCast, GameCast, SingleCast)
  , GameAction (CreateUser)
  , Player (MkPlayer, playerName)
  , TheGameError (InvalidJSONError, PlayerHandshakeFailed)
  , UserResponse (addressee, payload)
  )
import TheGame.Util (respondJSON)

conLoop :: TChan UserResponse -> TQueue (GameAction (Const ()), Player Identity) -> Player Identity -> Connection -> IO ()
conLoop respChan actChan user con = do
  mact :: Maybe (GameAction (Const ())) <- decode <$> receiveData con
  case mact of
    Nothing -> respondJSON con InvalidJSONError
    Just act -> do
      atomically do writeTQueue actChan (act, user)
      resp <- atomically do readTChan respChan
      let p = case resp.addressee of
            SingleCast addr -> addr == user
            GameCast addrs -> user `elem` addrs
            BroadCast -> True
      when p do respondJSON con resp.payload

handShakePlayer :: TQueue (GameAction (Const ()), Player Identity) -> Connection -> IO (Maybe (Player Identity))
handShakePlayer actChan con = do
  mplayerName :: Maybe (Player (Const ())) <- decode <$> receiveData con
  uuid <- UUID.nextRandom
  let mplayerWithID = (\player -> MkPlayer (Identity uuid) (player.playerName)) <$> mplayerName
  case mplayerWithID of
    Nothing -> respondJSON con PlayerHandshakeFailed
    Just playerWithID -> atomically do writeTQueue actChan (CreateUser, playerWithID)
  pure mplayerWithID

theGameWebSocket :: TChan UserResponse -> TQueue (GameAction (Const ()), Player Identity) -> ServerApp
theGameWebSocket globalRespChan actChan pending = do
  con <- acceptRequest pending
  respChan <- atomically $ dupTChan globalRespChan
  Just player <- handShakePlayer actChan con
  forever do conLoop respChan actChan player con
