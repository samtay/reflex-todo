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
  body ? backgroundColor "#dadada"
  ".grid" ? height (pct 100)
  ".column" ? maxWidth (px 450)

inlineClay :: MonadWidget t m => Css -> m ()
inlineClay = el "style" . text . toStrict . render
