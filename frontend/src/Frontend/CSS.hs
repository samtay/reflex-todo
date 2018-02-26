{-# LANGUAGE OverloadedStrings #-}
module Frontend.CSS
  ( inlineClay
  , mainStylesheet
  ) where

import Clay
import Data.Text.Lazy (toStrict)
import Reflex.Dom.Core (MonadWidget, el, text)

mainStylesheet :: Css
mainStylesheet = do
  body ? do
    color "#586e75"
    backgroundColor "#eeeeee"
  "h2.ui.center.aligned.icon.header" ? do
    paddingTop (px 20)
    color "#586e75"

inlineClay :: MonadWidget t m => Css -> m ()
inlineClay = el "style" . text . toStrict . render
