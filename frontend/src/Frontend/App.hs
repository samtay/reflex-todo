{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.App (app) where

import Control.Monad (forM_)

import Reflex.Dom

app :: MonadWidget t m => m ()
app = divClass "ui middle center aligned grid" . divClass "column" $ do
  -- Header
  elClass "h2" "ui teal header" $
    divClass "content" $ text "ToDo List"
  -- List items
  divClass "todo ui relaxed celled list" $ do
    forM_ (zip [1::Int ..] initItems) $ \(_, item) ->
      divClass "item" $ text item
  where
    initItems = [ "Send app store links to the FP meetup"
                , "RSVP to the FP meetup"
                , "Shower"
                ]

-- TODO
-- 1. Use 'shake' animation for deleting an item in edit mode
-- 2. Use 'glow' or 'bounce' or something for new item
-- 3. Use 'pulse' for showing new edits
-- 4. Shit that requires jquery
{-

    divClass "ui right action input" $ do
      rec ti <- textInput $ def { _textInputConfig_setValue = "" <$ submit
                                , _textInputConfig_attributes = constDyn ("class" =: "Add an item")
                                }
          (el, _) <- elClass' "div" "ui teal button" $ do
            elClass "i" "add icon" $ text "Add"
          let newItem = tag (value ti) $ domEvent Click el

-}
