{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Note this is my own orphan, but I'm avoiding TH in common
module Backend.Data where

import           Control.Exception      (bracket)
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Reader   (ask)
import           Control.Monad.State    (get, put, state)
import           Data.Typeable

import           Control.Lens           (over, _2)
import           Data.Acid              hiding (query, update)
import qualified Data.Acid              as Acid
import qualified Data.Acid.Local        as Acid
import           Data.IntMap            (IntMap)
import qualified Data.IntMap            as IntMap
import           Data.SafeCopy          (base, deriveSafeCopy)
import           Data.Text              (Text)

import           Common.Types
import           Common.Misc

--------------------------------------------------
-- Database definition and interacting functions
--------------------------------------------------
data TodoDb = TodoDb
  { _todoDb_items :: IntMap Item
  } deriving (Typeable)

-- | Class for issuing commands against a contextual db
class HasAcidState db m where
  askDb :: m (AcidState db)

query :: (EventState event ~ db, QueryEvent event, HasAcidState db m, MonadIO m)
      => event
      -> m (EventResult event)
query q = askDb >>= liftIO . \db -> Acid.query db q

update :: (EventState event ~ db, UpdateEvent event, HasAcidState db m, MonadIO m)
      => event
      -> m (EventResult event)
update u = askDb >>= liftIO . \db -> Acid.update db u

-- Acquire the acid state database, do something with it, and release it
withTodoDb :: (AcidState TodoDb -> IO a) -> IO a
withTodoDb = bracket (Acid.openLocalState (TodoDb mempty)) Acid.createCheckpointAndClose

--------------------------------------------------
-- Acid state query/update functions
--------------------------------------------------

-- | Query for all todo items
allItems :: Query TodoDb (IntMap Item)
allItems = _todoDb_items <$> ask

-- | Add a todo item. This should never fail, but keeping 'Maybe' context so
-- the API is all uniform here.
addItem :: Text -> Update TodoDb (Maybe (Int, Maybe Item))
addItem txt = do
  TodoDb items <- get
  let newItem = Item txt False
      (newKey, items') = intMapSnoc' newItem items
  put $ TodoDb items'
  return $ Just (newKey, Just newItem)

-- | Delete a todo item and return the appropriate patch, that is,
-- return Nothing if key was already deleted, Just patch if successful.
deleteItem :: Int -> Update TodoDb (Maybe (Int, Maybe Item))
deleteItem k = updateItemsWith $ \items ->
  let (mItem, items') = IntMap.updateLookupWithKey (\_ _ -> Nothing) k items
  in case mItem of
       Nothing -> (Nothing, items')
       Just _  -> (Just (k, Nothing), items')

-- | Complete a todo item and return the appropriate patch, that is,
-- return Nothing if key was already deleted, Just patch if successful.
completeItem :: Int -> Update TodoDb (Maybe (Int, Maybe Item))
completeItem k = updateItemsWith $ \items ->
  let (mItem, st) = IntMap.updateLookupWithKey (\_ i -> Just i {_item_completed = True}) k items
  in case mItem of
       Nothing -> (Nothing, st)
       Just i  -> (Just (k, Just i {_item_completed = True}), st)

-- | Generic update on todo items
updateItemsWith :: (IntMap Item -> (a, IntMap Item)) -> Update TodoDb a
updateItemsWith f = state $ \(TodoDb items) -> over _2 TodoDb (f items)

-- Template Haskell to make acidic types
deriveSafeCopy 0 'base ''Item
deriveSafeCopy 0 'base ''TodoDb
makeAcidic ''TodoDb ['allItems, 'addItem, 'deleteItem, 'completeItem]
