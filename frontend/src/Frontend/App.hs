{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Frontend.App (app) where

import qualified Data.Map       as Map
import           Reflex
import           Reflex.Dom

import           Common.Request
import           Common.Types
import           Frontend.Env
import           Frontend.Item

-- TODO if entire list update screws up scrolling, try 'listWithKeyShallowDiff'
app :: TodoWidget t m (Event t TodoRequest)
app = divClass "ui text container" $ divClass "ui segments" $ do
  let items = pure mempty
  -- Uncompleted items
  completes <- divClass "ui segment" $
    divClass "ui big ordered relaxed divided list" $ do
      listViewWithKey (Map.filter (not . _item_completed) <$> items) drawItem
  -- New item
  newItem <- drawItemInput
  -- Completed items
  deletes <- divClass "ui segment" $
    divClass "ui big relaxed divided list" $ do
      listViewWithKey (Map.filter _item_completed <$> items) drawItem
  -- Handle requests via Reflex !
  return never
