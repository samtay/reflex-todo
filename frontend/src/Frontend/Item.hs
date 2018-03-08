{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
module Frontend.Item
  ( drawItem
  , drawItemInput
  ) where

import           Control.Monad  ((<=<))
import           Data.Semigroup ((<>))

import           Data.Text      (Text)
import qualified Data.Text      as T
import           Reflex.Dom

import           Common.Types

drawItem :: MonadWidget t m => Int -> Dynamic t Item -> m (Event t (Maybe Item))
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
