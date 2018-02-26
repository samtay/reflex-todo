{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Frontend.App (app) where

import           Control.Monad  (void)
import           Data.Semigroup ((<>))
import           Data.Text      (Text)
import qualified Data.Text      as T

import           Reflex.Dom

app :: MonadWidget t m => m ()
app = do
  -- Header
  elClass "h2" "ui center aligned icon header" $ do
    elClass "i" "circular list alternate outline icon" $ blank
    text "ToDo List"
  -- List items
  divClass "ui text container" $ do
    divClass "ui segments" $ do
      let newItem = never
      items <- foldDyn snoc initItems newItem
      let indexedItems = fmap (zip [1..]) items
      void $ simpleList indexedItems drawItem
  where
    initItems = [ "Send app store links to the FP meetup"
                , "RSVP to the FP meetup"
                , "Shower"
                ]

drawItem :: MonadWidget t m => Dynamic t (Int, Text) -> m ()
drawItem itemD = void . dyn . ffor itemD $ \(ix, item) -> do
  divClass "ui segment" $ text $ (T.pack $ show ix) <> ". " <> item

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

snoc :: a -> [a] -> [a]
snoc x xs = xs ++ [x]
