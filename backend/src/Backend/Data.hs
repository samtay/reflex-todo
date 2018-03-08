{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-- Note this is my own orphan, but I'm avoiding TH in common
module Backend.Data where

import           Control.Monad.Reader (ask)
import           Control.Monad.State  (modify)
import           Data.Typeable

import           Data.Acid
import           Data.IntMap          (IntMap)
import qualified Data.IntMap          as IntMap
import           Data.SafeCopy        (base, deriveSafeCopy)
import           Data.Text            (Text)

import           Backend.Util         (mapSnoc)
import           Common.Types

data TodoDb = TodoDb
  { _todoDb_items :: IntMap Item
  } deriving (Typeable)

-- | Open state by resuming log or starting with empty list
openState :: IO (AcidState TodoDb)
openState = openLocalState (TodoDb IntMap.empty)

-- | Query for all todo items
allItems :: Query TodoDb (IntMap Item)
allItems = _todoDb_items <$> ask

-- | Add a todo item
addItem :: Text -> Update TodoDb ()
addItem txt = updateItemsWith $ \items ->
  let newItem = Item { _item_text = txt
                     , _item_completed = False
                     }
  in fst $ mapSnoc newItem items

-- | Delete a todo item
deleteItem :: Int -> Update TodoDb ()
deleteItem k = updateItemsWith $ IntMap.delete k

-- | Complete a todo item
completeItem :: Int -> Update TodoDb ()
completeItem k = updateItemsWith $ IntMap.adjust (\item -> item {_item_completed = True}) k

-- | Generic update on todo items
updateItemsWith :: (IntMap Item -> IntMap Item) -> Update TodoDb ()
updateItemsWith f = modify $ \(TodoDb items) -> TodoDb (f items)

-- Template Haskell to make acidic types
deriveSafeCopy 0 'base ''Item
deriveSafeCopy 0 'base ''TodoDb
makeAcidic ''TodoDb ['allItems, 'addItem, 'deleteItem, 'completeItem]
