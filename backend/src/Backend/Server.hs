{-# LANGUAGE OverloadedStrings #-}
module Backend.Server
  ( runServer
  ) where

import           Control.Monad                  ((>=>))

import qualified Network.Wai                    as Wai
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as Wai.WS
import qualified Network.WebSockets             as WS

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
