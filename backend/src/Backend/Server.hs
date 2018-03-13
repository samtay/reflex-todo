{-# LANGUAGE OverloadedStrings #-}
module Backend.Server
  ( runServer
  , NetworkState
  ) where

import           Control.Exception              (finally)

import           Control.Concurrent.MVar        (MVar)
import qualified Control.Concurrent.MVar        as Concurrent
import           Data.IntMap                    (IntMap)
import qualified Data.IntMap                    as IntMap
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as Wai.WS
import qualified Network.WebSockets             as WS

import           Backend.Util                   (mapSnoc)

type NetworkState = MVar (IntMap WS.Connection)
-- TODO obviously get rid of this
type AppRunner = NetworkState -> Int -> WS.Connection -> IO ()

-- | Run server on given port (websocket + http)
runServer :: Int -> AppRunner -> IO ()
runServer port app = do
  state <- Concurrent.newMVar IntMap.empty
  Warp.run port $ Wai.WS.websocketsOr
    WS.defaultConnectionOptions -- ^ Compression disabled / lenient unicode decoding
    (wsApp app state)           -- ^ App for websocket requests
    httpApp                     -- ^ App for non-websocket requests

-- | Websocket application
wsApp :: AppRunner -> NetworkState -> WS.ServerApp
wsApp app conns pendingConn = do
  conn <- WS.acceptRequest pendingConn -- ^ Accept the pending connection
  clientId <- connectClient conn conns -- ^ Register the new connection in our state
  WS.forkPingThread conn 30            -- TODO why?
  finally
    (app conns clientId conn)          -- ^ Run app for this client connection
    (disconnectClient clientId conns)  -- ^ Remove connection after disconnect or exception
  where listen _ _ _ = return ()

-- | Http application
-- TODO serve ghcjs on this server
httpApp :: Wai.Application
httpApp _ respond = respond $
  Wai.responseLBS Http.status400 [] "This server only accepts websocket requests"

-- | Add a client connection to the app state
connectClient :: WS.Connection -> NetworkState -> IO Int
connectClient conn conns = Concurrent.modifyMVar conns $ \conns' -> return $
  mapSnoc conn conns'

-- | Remove a client connection from the app state
disconnectClient :: Int -> NetworkState -> IO ()
disconnectClient clientId conns = Concurrent.modifyMVar_ conns $ \conns' -> return $
  IntMap.delete clientId conns'
