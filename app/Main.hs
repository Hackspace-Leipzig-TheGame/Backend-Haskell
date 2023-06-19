module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TChan, TQueue, newBroadcastTChanIO, newTQueueIO)
import Data.Functor.Const (Const)
import Data.Functor.Identity (Identity)
import Network.HTTP.Types (status400)
import Network.Wai (Application, Response, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import TheGame qualified
import TheGame.Handlers (handleActs)
import TheGame.Types (GameAction, Player, UserResponse)

waiApp :: TChan UserResponse -> TQueue (GameAction (Const ()), Player Identity) -> Application
waiApp globalRespChan actChan = websocketsOr defaultConnectionOptions (TheGame.theGameWebSocket globalRespChan actChan) backupApp
 where
  backupApp :: forall a r. a -> (Response -> r) -> r
  backupApp _req res = res do
    responseLBS status400 [] "This is a websocket and you didn't make a websocket request"

main :: IO ()
main = do
  globalRespChan <- newBroadcastTChanIO
  globalActQueue <- newTQueueIO
  _tid <- forkIO (handleActs globalRespChan globalActQueue)
  Warp.run 8080 (waiApp globalRespChan globalActQueue)
