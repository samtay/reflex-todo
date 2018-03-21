{-# LANGUAGE OverloadedStrings #-}
module Backend.Server
  ( runServer
  , HasConnection(..)
  , sendJSONData
  , receiveJSONData
  ) where

import           Control.Monad                  ((>=>))
import           Control.Monad.IO.Class         (MonadIO (..))

import           Data.Aeson
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as Wai.WS
import qualified Network.WebSockets             as WS

-- | Small typeclass for implicitly using the contextual connection
class MonadIO m => HasConnection m where
  askConnection :: m (WS.Connection)
  sendTextData :: WS.WebSocketsData a => a -> m ()
  sendTextData a = askConnection >>= liftIO . flip WS.sendTextData a
  receiveData :: WS.WebSocketsData a => m a
  receiveData = askConnection >>= liftIO . WS.receiveData

sendJSONData :: (ToJSON a, HasConnection m) => a -> m ()
sendJSONData = sendTextData . encode

receiveJSONData :: (FromJSON a, HasConnection m) => m (Either String a)
receiveJSONData = eitherDecode <$> receiveData

-- | Run server on given port (websocket + http)
runServer
  :: Int
  -> (WS.Connection -> IO ())
  -> IO ()
runServer port wsApp = do
  Warp.run port $ Wai.WS.websocketsOr
    WS.defaultConnectionOptions -- Compression disabled / lenient unicode decoding
    (WS.acceptRequest >=> \conn -> WS.forkPingThread conn 30 >> wsApp conn) -- App for websocket requests
    httpApp -- App for non-websocket requests

-- | Http application
--
-- Serve up static ghcjs output
httpApp :: Wai.Application
httpApp = Static.staticApp $ Static.defaultFileServerSettings "frontend.jsexe"

