{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Backend.App where

import           Control.Concurrent.STM
import           Control.Lens           (iforM_)
import           Control.Monad.Reader
import           Data.Acid              (AcidState)
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
import           Data.Map               (Map)
import qualified Data.Map               as Map

import           Backend.Data
import           Common.Api
import           Common.Misc

-- App context available for reading
data AppContext c = AppContext
  { _app_clientId   :: Int              -- ^ Identifier for this client connection
  , _app_connection :: c                -- ^ Parameterized connection type
  , _app_db         :: AcidState TodoDb -- ^ Reference to db
  , _app_clients    :: TVar (IntMap c)  -- ^ Shared access to list of active client connections
  }

instance MonadReader (AppContext c) m => HasAcidState TodoDb m where
  askDb = asks _app_db

-- Typeclass for the notion of sending and receiving data across some
-- connection handle
class HasConnection m where
  type Connection m :: *
  sendData :: Connection m -> WebSocketDataDown -> m ()
  receiveData :: Connection m -> m (Either String WebSocketDataUp)

-- | Add a new connection to the tvar map, return the new connection's client ID
connect :: (TVar (IntMap c)) -> c -> IO Int
connect csRef c = atomically $ do
  cs <- readTVar csRef
  let (cid, cs') = intMapSnoc' c cs
  writeTVar csRef cs'
  return cid

-- | Remove a client from the list by ID
disconnect :: (TVar (IntMap c)) -> Int -> IO ()
disconnect csRef cid = atomically $ modifyTVar csRef $ IntMap.delete cid

-- | Send message to this app context's connection handle
tellSelf
  :: (MonadIO m, HasConnection m, MonadReader (AppContext (Connection m)) m)
  => WebSocketDataDown -- ^ Message to send
  -> m ()
tellSelf msg = asks _app_connection >>= \conn -> sendData conn msg

-- | Send message to everyone except this app context's connection handle
tellOthers
  :: (MonadIO m, HasConnection m, MonadReader (AppContext (Connection m)) m)
  => WebSocketDataDown -- ^ Message to send
  -> m ()
tellOthers msg = asks _app_clientId >>= \cid -> tell (/= cid) msg

-- | Send message to every connection handle
tellAll
  :: (MonadIO m, HasConnection m, MonadReader (AppContext (Connection m)) m)
  => WebSocketDataDown -- ^ Message to send
  -> m ()
tellAll = tell (const True)

-- | Send message to all connection handles that satisfy the predicate
tell
  :: (MonadIO m, HasConnection m, MonadReader (AppContext (Connection m)) m)
  => (Int -> Bool) -- ^ Predicate against client ID
  -> WebSocketDataDown -- ^ Message to send
  -> m ()
tell p msg = do
  conns <- liftIO . readTVarIO =<< asks _app_clients
  iforM_ conns $ \cid conn ->
    when (p cid) $ sendData conn msg

-- | Execute a TodoRequest and, based on the result, send messages to the
-- appropriate clients
handleTodoRequest
  :: (MonadIO m, HasConnection m, MonadReader (AppContext (Connection m)) m)
  => TodoRequest
  -> m ()
handleTodoRequest req = do
  res <- case req of TodoRequest_Create txt  -> update $ AddItem txt
                     TodoRequest_Complete ix -> update $ CompleteItem ix
                     TodoRequest_Delete ix   -> update $ DeleteItem ix
  case res of
    Nothing ->
      tellSelf . WebSocketDataDown_Response $ Left "Oops, someone already deleted that item"
    Just patch -> do
      tellSelf . WebSocketDataDown_Response $ Right ()
      tellAll . WebSocketDataDown_Listen . TodoListen_ListPatch . uncurry Map.singleton $ patch

-- The "main" app function per connection thread
app :: (MonadIO m, HasConnection m, MonadReader (AppContext (Connection m)) m) => m ()
app = do
  -- First send the full list
  query AllItems >>= tellSelf . WebSocketDataDown_Listen . TodoListen_ListFull . toMap
  -- Handle requests from this client's websocket connection
  conn <- asks _app_connection
  forever $ receiveData conn >>= \case
    Left err  -> liftIO $ putStrLn $ "Decoding error at websocket: " ++ err
    Right (WebSocketDataUp_Request req) -> handleTodoRequest req

-- | Transform IntMap to Map Int
toMap :: IntMap a -> Map Int a
toMap = Map.fromList . IntMap.toList
