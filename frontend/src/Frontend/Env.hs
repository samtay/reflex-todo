{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Frontend.Env
  ( runApp
  , Connection(..)
  , TodoEnv(..)
  , TodoWidget
  ) where

import           Control.Monad.Reader

import           Control.Lens         (Prism', (^?))
import           Data.Text            (Text)
import           Reflex.Dom.Core
import           Reflex.Dom.WebSocket as WS

import           Common.Api

-- | Our widget type has reader capabilities tacked on
type TodoWidget t m a = MonadWidget t m => ReaderT (TodoEnv t) m a

-- | Connection state
data Connection
  = Connection_Connected    -- ^ Active websocket connection
  | Connection_Reconnecting -- ^ Trying to reconnect (5 second timeout)
  | Connection_Disconnected -- ^ Disconnected
  deriving (Eq)

-- | Our env for reading at runtime
data TodoEnv t = TodoEnv
  { _todoEnv_connection :: Dynamic t Connection
  , _todoEnv_listen     :: Event t TodoListen
  , _todoEnv_response   :: Event t TodoResponse
  }

-- | Run our main TodoWidget by creating the environment for it.
-- Handles opening the web socket connection and encoding up requests,
-- decoding down responses and other state updates.
runApp
  :: (MonadWidget t m)
  => TodoWidget t m (Event t [TodoRequest])
  -> m ()
runApp app = do
  rec ws <- WS.jsonWebSocket appHost $
        def { _webSocketConfig_send = encodeUp <$> leftmost
              [ gate (current connected) request
              , tag (current buffer) (ffilter id $ updated connected)
              ]
            , _webSocketConfig_reconnect = False
            }
      (connected, connection) <- getConnectionState ws
      buffer <- foldDyn (++) [] $ gate (not <$> current connected) request
      let recv = _webSocket_recv ws
          env = TodoEnv { _todoEnv_connection = connection
                        , _todoEnv_listen = fmapMaybe (decodeDown _WebSocketDataDown_Listen) recv
                        , _todoEnv_response = fmapMaybe (decodeDown _WebSocketDataDown_Response) recv
                        }
      request <- runReaderT app env
  return ()
  where
    encodeUp :: [TodoRequest] -> [WebSocketDataUp]
    encodeUp = fmap WebSocketDataUp_Request

    decodeDown :: Prism' WebSocketDataDown a -> Maybe WebSocketDataDown -> Maybe a
    decodeDown prism mdown = mdown >>= (^? prism)

-- | Calculates current connection status from 'RawWebSocket'
getConnectionState
  :: MonadWidget t m
  => RawWebSocket t a
  -> m ( Dynamic t Bool       -- ^ Connected
       , Dynamic t Connection -- ^ Connection status (possibly attempting reconnect)
       )
getConnectionState ws = do
  let open = _webSocket_open ws
      close = () <$ _webSocket_close ws
  pb <- getPostBuild
  checkConnection <- delay 10 $ leftmost [close, pb]
  connected <- holdDyn False $ leftmost
    [ True <$ open
    , False <$ close
    ]
  connection <- holdUniqDyn <=< holdDyn Connection_Reconnecting $ leftmost
    [ Connection_Connected <$ open
    , Connection_Reconnecting <$ close
    , Connection_Connected <$ gate (current connected) checkConnection
    , Connection_Disconnected <$ gate (not <$> current connected) checkConnection
    ]
  return (connected, connection)

appHost :: Text
appHost = "wss://reflex-todo.herokuapp.com"
