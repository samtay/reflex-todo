{-# LANGUAGE OverloadedStrings #-}
module Backend.Server (runServer) where

import           Control.Exception              (finally)

import           Control.Concurrent.MVar        (MVar)
import qualified Control.Concurrent.MVar        as Concurrent
import           Data.Map                       (Map)
import qualified Data.Map                       as Map
import qualified Network.HTTP.Types             as Http
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as Wai.WS
import qualified Network.WebSockets             as WS

runServer :: Int -> IO ()
runServer port = do
  state <- Concurrent.newMVar Map.empty
  Warp.run port $ Wai.WS.websocketsOr
    WS.defaultConnectionOptions -- ^ Options define what to do on pong
    (wsApp state)               -- ^ App for websocket requests
    httpApp                     -- ^ App for non-websocket requests

wsApp :: MVar (Map Int WS.Connection) -> WS.ServerApp
wsApp conns pendingConn = do
  conn <- WS.acceptRequest pendingConn -- ^ Accept the pending connection
  clientId <- connectClient conn conns -- ^ Register the new connection in our state
  WS.forkPingThread conn 30            -- TODO why?
  finally
    (listen clientId conn conns)       -- ^ Listener TODO separate concerns, pass in app func / broadcasting
    (disconnectClient clientId conns)  -- ^ Remove connection after disconnect or exception
  where listen _ _ _ = return ()

-- TODO serve ghcjs on this server
httpApp :: Wai.Application
httpApp _ respond = respond $
  Wai.responseLBS Http.status400 [] "This server only accepts websocket requests"

connectClient :: WS.Connection -> MVar (Map Int WS.Connection) -> IO Int
connectClient conn conns = Concurrent.modifyMVar conns $ \conns' -> return $
  mapSnoc conn conns'

disconnectClient :: Int -> MVar (Map Int WS.Connection) -> IO ()
disconnectClient clientId conns = Concurrent.modifyMVar_ conns $ \conns' -> return $
  Map.delete clientId conns'

mapSnoc :: a -> Map Int a -> (Map Int a, Int) -- ^ The new map and new key
mapSnoc a xs = case Map.maxViewWithKey xs of
                 Just ((k, _), _) -> (Map.insert (succ k) a xs, succ k)
                 _                -> (Map.singleton 1 a, 1)
