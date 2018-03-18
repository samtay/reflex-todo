{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Frontend.App (app) where

import           Control.Monad.Reader (asks)

import qualified Data.Map       as Map
import           Reflex
import           Reflex.Dom

import           Common.Api
import           Common.Types
import           Frontend.Env
import           Frontend.Item

-- TODO if entire list update screws up scrolling, try 'listWithKeyShallowDiff'
app :: TodoWidget t m (Event t [TodoRequest])
app = divClass "ui text container" $ divClass "ui segments" $ do
  listUpdates <- asks _todoEnv_listen
  items <- fmap (traceDyn "items") $ foldDyn ($) mempty . ffor listUpdates $ \case
    TodoListen_ListFull l -> const l
    TodoListen_ListPatch p -> applyMap p
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
  -- Return request events
  return $ mergeWith (++) [ (:[]) . TodoRequest_Create <$> newItem
                          , fmap TodoRequest_Complete <$> Map.keys <$> completes
                          , fmap TodoRequest_Delete <$> Map.keys <$> deletes
                          ]
