{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
module Frontend.App (app) where

import           Data.Semigroup ((<>))

import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Reflex
import           Reflex.Dom

import           Common.Types
import           Frontend.Item

app :: MonadWidget t m => Dynamic t (Map Int Item) -> m ()
app items = do
  divClass "main" $ do
    -- Header
    elClass "h2" "ui center aligned icon header" $ do
      elClass "i" "circular list alternate outline icon" $ blank
      text "ToDo List"
    -- List items
    divClass "ui text container" $ divClass "ui segments" $ do
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
      return ()
  -- Footer
  divClass "footer" $ divClass "ui center aligned container" $ do
    elAttr "a" ("href" =: "https://github.com/samtay/reflex-todo" <> "target" =: "_blank") $
      elAttr "button" ("class" =: "ui circular icon basic button") $ do
        elClass "i" "github icon" blank
