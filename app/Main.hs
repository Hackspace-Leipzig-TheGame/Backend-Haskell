module Main where

import Control.Concurrent.STM (TChan, TQueue, newBroadcastTChanIO, newTQueueIO)
import Data.Functor.Const (Const)
import Network.HTTP.Types (status400)
import Network.Wai (Application, Response, responseLBS)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets (defaultConnectionOptions)
import TheGame qualified
import TheGame.Types (GameAction, UserResponse)

waiApp :: TChan UserResponse -> TQueue (GameAction (Const ())) -> Application
waiApp globalRespChan actChan = websocketsOr defaultConnectionOptions (TheGame.theGameWebSocket globalRespChan actChan) backupApp
 where
  backupApp :: forall a r. a -> (Response -> r) -> r
  backupApp _req res = res do responseLBS status400 [] "This is a websocket and you didn't make a websocket request"

main :: IO ()
main = do
  globalRespChan <- newBroadcastTChanIO
  globalActQueue <- newTQueueIO
  Warp.run 8080 (waiApp globalRespChan globalActQueue)
