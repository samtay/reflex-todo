{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Backend.Master
  ( Master(..)
  , HasMaster(..)
  , withMaster
  , subscribe
  , handleRequest
  ) where

import           Control.Concurrent.STM       (atomically, newBroadcastTChan)
import           Control.Concurrent.STM.TChan
import           Control.Monad.IO.Class       (MonadIO (..))
import           Data.Acid                    (AcidState)
import qualified Data.Map                     as Map

import           Backend.Data
import           Common.Api

-- TODO should master contain a TVar (IntMap Client) ? everyone else seems to do this..
-- If so add the field here and a connect function..
data Master = Master
  { _master_broadcast :: TChan TodoListen -- ^ Channel to send updates to clients
  , _master_db        :: AcidState TodoDb
  }

class HasMaster m where
  askMaster :: m Master

instance (MonadIO m, HasMaster m) => HasAcidState TodoDb m where
  askDb = _master_db <$> askMaster

withMaster :: (Master -> IO a) -> IO a
withMaster f = withTodoDb $ \db -> do
  broadcast <- atomically newBroadcastTChan
  f $ Master broadcast db

handleRequest :: (MonadIO m, HasMaster m) => TodoRequest -> m TodoResponse
handleRequest req = do
  res <- case req of TodoRequest_Create txt  -> update $ AddItem txt
                     TodoRequest_Complete ix -> update $ CompleteItem ix
                     TodoRequest_Delete ix   -> update $ DeleteItem ix
  case res of
    Nothing -> return $ Left "Oops, someone already deleted that item"
    Just patch -> do
      broadcast <- _master_broadcast <$> askMaster
      liftIO . atomically . writeTChan broadcast .
        TodoListen_ListPatch . uncurry Map.singleton $ patch
      return $ Right ()

-- | Start a subscription, i.e. get a duplicate of the broadcast channel
-- TODO consider map of tchans instead of broadcast, so that we can easily send
-- FullList to single client
subscribe :: Master -> IO (TChan TodoListen)
subscribe master = atomically . dupTChan $ _master_broadcast master


