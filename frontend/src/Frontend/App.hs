{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
module Frontend.App (app) where

import           Control.Monad  ((<=<))
import           Data.Semigroup ((<>))

import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Map.Misc  (applyMap)
import           Data.Text      (Text)
import qualified Data.Text      as T
import           Reflex
import           Reflex.Dom

import           Common.Types

app :: MonadWidget t m => m ()
app = do
  divClass "main" $ do
    -- Header
    elClass "h2" "ui center aligned icon header" $ do
      elClass "i" "circular list alternate outline icon" $ blank
      text "ToDo List"
    -- List items
    divClass "ui text container" $ divClass "ui segments" $ do
      rec items <- foldDyn ($) initItems $ leftmost [ mapSnoc . mkItem <$> newItem
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
  -- Footer
  divClass "footer" $ divClass "ui center aligned container" $ do
      elAttr "a" ("href" =: "https://github.com/samtay/reflex-todo") $
        elAttr "button" ("class" =: "ui circular icon basic button") $ do
          elClass "i" "github icon" blank
  where
    mkItem t = Item t False
    initItems = Map.fromList $ zip [1..] $ fmap mkItem
      [ "Send app store links to the FP meetup"
      , "RSVP to the FP meetup"
      , "Shower"
      ]

drawItem :: MonadWidget t m => k -> Dynamic t Item -> m (Event t (Maybe Item))
drawItem _ item = switchHold never <=< dyn . ffor item $ \(Item txt completed) -> do
  divClass ("item" <> if completed then " completed" else "") $ do
    (complete, delete) <- divClass "right floated content" $ do
      complete' <- if completed then return never else fmap (domEvent Click . fst) $
        elClass' "i" "icon link check teal" blank
      delete' <- if not completed then return never else fmap (domEvent Click . fst) $
        elClass' "i" "icon link trash alternate" blank
      return (complete', delete')
    divClass "content" $ text txt
    return $ leftmost [ Just (Item txt True) <$ complete
                      , Nothing <$ delete
                      ]

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
