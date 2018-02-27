{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
module Frontend.App (app) where

import           Data.Map      (Map)
import qualified Data.Map      as Map
import           Data.Map.Misc (applyMap)
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Reflex.Dom

app :: MonadWidget t m => m ()
app = do
  -- Header
  elClass "h2" "ui center aligned icon header" $ do
    elClass "i" "circular list alternate outline icon" $ blank
    text "ToDo List"
  -- List items
  divClass "ui text container" $ divClass "ui segments" $ do
    rec items <- foldDyn ($) initItems $ leftmost [ mapSnoc <$> newItem
                                                  , applyMap <$> update
                                                  ]
        update <- divClass "ui segment" $
          divClass "ui big ordered relaxed divided list" $ do
            listViewWithKey items drawItem
        newItem <- drawItemInput
    return ()
  where
    initItems = Map.fromList $ zip [1..]
      [ "Send app store links to the FP meetup"
      , "RSVP to the FP meetup"
      , "Shower"
      ]

drawItem :: MonadWidget t m => k -> Dynamic t Text -> m (Event t (Maybe Text))
drawItem _ item = divClass "item" $ do
  (trashIcon, _) <- divClass "right floated content" $
    elClass' "i" "icon link trash alternate" blank
  divClass "content" $ dynText item
  return $ Nothing <$ domEvent Click trashIcon

drawItemInput :: MonadWidget t m => m (Event t Text)
drawItemInput = divClass "ui segment fluid action input" $ do
  rec ti <- textInput $ def & textInputConfig_setValue .~ ("" <$ submit)
                            & textInputConfig_attributes .~ constDyn ("placeholder" =: "Add an item")
      (btn, _) <- elClass' "div" "ui teal right labeled icon button" $ do
        elClass "i" "add icon" blank
        text "Add"
      let submit = leftmost [ domEvent Click btn
                            , keypress Enter ti
                            ]
  return $ ffilter (not . T.null) $ tag (current $ value ti) submit

mapSnoc :: a -> Map Int a -> Map Int a
mapSnoc a xs = case Map.toDescList xs of
                 (k, _) : _ -> Map.insert (succ k) a xs
                 _          -> Map.singleton 1 a

-- TODO
-- 1. Use 'shake' animation for deleting an item in edit mode
-- 2. Use 'glow' or 'bounce' or something for new item
-- 3. Use 'pulse' for showing new edits
-- 4. Shit that requires jquery
