module TheGame.Util (respondJSON, liftWriteTChan) where

import Control.Concurrent.STM (TChan, atomically, writeTChan)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Aeson (ToJSON, encode)
import Network.WebSockets (Connection, sendTextData)

respondJSON :: (ToJSON a) => Connection -> a -> IO ()
respondJSON con = sendTextData con . encode

liftWriteTChan :: (MonadIO m) => TChan a -> a -> m ()
liftWriteTChan chan = liftIO . atomically . writeTChan chan
