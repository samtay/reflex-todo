{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
module Frontend.Static (app) where

import qualified Data.Map      as Map
import           Data.Map.Misc (applyMap)
import           Reflex
import           Reflex.Dom

import           Common.Misc
import           Common.Types
import           Frontend.Item

app :: MonadWidget t m => m ()
app = divClass "ui text container" $ divClass "ui segments" $ do
  let mkItem t = Item t False
  rec items <- foldDyn ($) mempty $ leftmost [ mapSnoc . mkItem <$> newItem
                                             , applyMap <$> updateUncompletes
                                             , applyMap <$> updateCompletes
                                             ]
      -- Uncompleted items
      updateUncompletes <- divClass "ui segment" $
        divClass "ui big ordered relaxed divided list" $ do
          listViewWithKey (Map.filter (not . _item_completed) <$> items) drawItem
      -- New item
      newItem <- drawItemInput
      -- Completed items
      updateCompletes <- divClass "ui segment" $
        divClass "ui big relaxed divided list" $ do
          listViewWithKey (Map.filter _item_completed <$> items) drawItem
  return ()
