{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
module Frontend.App (app) where

import           Control.Monad  (void)
import           Data.Text      (Text)

import           Reflex.Dom

app :: MonadWidget t m => m ()
app = do
  -- Header
  elClass "h2" "ui center aligned icon header" $ do
    elClass "i" "circular list alternate outline icon" $ blank
    text "ToDo List"
  -- List items
  divClass "ui text container" $ divClass "ui segments" $ do
    rec items <- foldDyn snoc initItems newItem
        divClass "ui segment" $
          divClass "ui big ordered relaxed divided list" $ do
            void $ simpleList items drawItem
        newItem <- drawItemInput
    return ()
  where
    initItems = [ "Send app store links to the FP meetup"
                , "RSVP to the FP meetup"
                , "Shower"
                ]

drawItem :: MonadWidget t m => Dynamic t Text -> m ()
drawItem = divClass "item" . divClass "content" . dynText

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

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]

-- TODO
-- 1. Use 'shake' animation for deleting an item in edit mode
-- 2. Use 'glow' or 'bounce' or something for new item
-- 3. Use 'pulse' for showing new edits
-- 4. Shit that requires jquery
