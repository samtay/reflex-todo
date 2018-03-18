{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Backend.Client
  ( clientApp
  , Client(..)
  ) where

import           Control.Concurrent           (forkIO)

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Monad.Reader
import           Data.Aeson
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import qualified Network.WebSockets           as WS

import           Backend.Data
import           Backend.Master
import           Backend.Server
import           Common.Api

type ClientApp = ReaderT Client IO

data Client = Client
  { _client_connection :: WS.Connection
  , _client_listen     :: TChan TodoListen
  , _client_master     :: Master
  }

instance (MonadIO m, MonadReader Client m) => HasConnection m where
  askConnection = asks _client_connection

instance (MonadReader Client m) => HasMaster m where
  askMaster = asks _client_master

clientApp :: ClientApp ()
clientApp = do
  -- First send the full list
  query AllItems >>= sendJSONData . WebSocketDataDown_Listen . TodoListen_ListFull . toMap
  -- Relay listener updates from all clients
  Client conn listen _ <- ask
  liftIO . void . forkIO . forever $
    atomically (readTChan listen) >>= WS.sendTextData conn . encode . WebSocketDataDown_Listen
  -- Handle requests from this client's websocket connection
  forever $ receiveJSONData >>= \case
    Left err  -> liftIO $ putStrLn $ "Decoding error at websocket: " ++ err
    Right req -> handleRequest req >>= sendJSONData . WebSocketDataDown_Response

toMap :: IntMap a -> Map Int a
toMap = Map.fromList . IntMap.toList
