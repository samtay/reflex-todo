{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Backend.App
  ( app
  , AppState(..)
  ) where

import           Control.Concurrent           (forkIO)

import           Control.Concurrent.STM       (atomically)
import           Control.Concurrent.STM.TChan
import           Control.Monad.Reader
import           Data.Acid                    (AcidState)
import           Data.Aeson
import           Data.IntMap                  (IntMap)
import qualified Data.IntMap                  as IntMap
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Text                    (Text)
import qualified Network.WebSockets           as WS

import           Backend.Data
import           Common.Request

type App = ReaderT AppState IO

-- TODO if instead the hasacidstate instance refers to a parent thread, then the parent thread can get the result of the update action (e.g. new data patch) and send to the broadcast, sending the error/success response to the caller!
data AppState = AppState
  { _appState_connection :: WS.Connection
  , _appState_todoDb     :: AcidState TodoDb -- ^ Perhaps keep this in parent and wrap typeclass...
  , _appState_broadcast  :: TChan TodoListen -- ^ Send updates -- TODO consider moving this out of client
  , _appState_listen     :: TChan TodoListen -- ^ Receive updates
  }

-- TODO move this somewhere else, perhaps resolve circ dep with Backend.Server
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

instance (MonadIO m, MonadReader AppState m) => HasConnection m where
  askConnection = asks _appState_connection

instance (MonadIO m, MonadReader AppState m) => HasAcidState TodoDb m where
  askDb = asks _appState_todoDb

app :: App ()
app = do
  AppState conn _ broadcast listen <- ask
  -- First send the full list
  -- TODO consider sendListenData / sendResponseData small wrappers
  query AllItems >>= sendJSONData . WebSocketDataDown_Listen . TodoListen_ListFull . toMap
  -- Relay state updates from other clients
  liftIO . void . forkIO . forever $
    atomically (readTChan listen) >>= WS.sendTextData conn . encode . WebSocketDataDown_Listen
  -- Handle requests from this client's websocket connection
  forever $ do
    up <- receiveJSONData
    case up of
      Left err -> liftIO $ putStrLn $ "Decoding error at websocket: " ++ err
      Right req -> do
        result <- case req of
          TodoRequest_Create txt  -> Right <$> update (AddItem txt)
          TodoRequest_Complete ix -> Right <$> update (CompleteItem ix)
          TodoRequest_Delete ix   -> Right <$> update (DeleteItem ix)
        sendJSONData $ WebSocketDataDown_Response result
        query AllItems >>= liftIO . atomically . writeTChan broadcast . TodoListen_ListFull . toMap
  -- TODO remove ^ after refactor

toMap :: IntMap a -> Map Int a
toMap = Map.fromList . IntMap.toList
