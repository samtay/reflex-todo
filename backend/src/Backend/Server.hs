{-# LANGUAGE OverloadedStrings #-}
module Backend.Server
  ( runServer
  ) where

import           Control.Monad                  ((>=>))

import           Control.Concurrent.STM         (atomically)
import           Control.Concurrent.STM.TChan   (TChan, dupTChan)
import           Control.Monad.Reader           (runReaderT)
import           Data.Acid                      (AcidState)
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as Wai.WS
import qualified Network.WebSockets             as WS

import           Backend.App                    (AppState (..), app)
import           Backend.Data                   (TodoDb)
import           Common.Request                 (TodoListen)

-- | Run server on given port (websocket + http)
runServer
  :: Int
  -> AcidState TodoDb
  -> TChan TodoListen
  -> IO ()
runServer port state broadcast = do
  Warp.run port $ Wai.WS.websocketsOr
    WS.defaultConnectionOptions -- Compression disabled / lenient unicode decoding
    (WS.acceptRequest >=> makeClient state broadcast >=> runReaderT app) -- App for websocket requests
    httpApp -- App for non-websocket requests

-- | Http application
-- TODO serve ghcjs on this server
httpApp :: Wai.Application
httpApp _ respond = respond $
  Wai.responseLBS Http.status400 [] "This server only accepts websocket requests"

-- | Add a client connection
makeClient
  :: AcidState TodoDb
  -> TChan TodoListen
  -> WS.Connection
  -> IO AppState
makeClient state broadcast conn = do
  WS.forkPingThread conn 30 -- Keep connections open past 60s proxy timeout
  listener <- atomically $ dupTChan broadcast -- Add a listener for this client
  return $ AppState { _appState_connection = conn
                    , _appState_todoDb = state
                    , _appState_broadcast = broadcast
                    , _appState_listen = listener
                    }
