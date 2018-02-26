{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Data.Semigroup                   ((<>))

import qualified Data.Text                        as T
import qualified Language.Javascript.JSaddle.Warp as JSaddle.Warp
import           Reflex.Dom
import qualified Reflex.Dom.Core

import qualified Frontend.App                     as App
import qualified Frontend.CSS as CSS

warp :: IO ()
warp = JSaddle.Warp.run 3911 $ Reflex.Dom.Core.mainWidgetWithHead headWidget App.app

main :: IO ()
main = Reflex.Dom.mainWidgetWithHead headWidget App.app

headWidget :: MonadWidget t m => m ()
headWidget = do
  el "title" $ text "Reflex ToDo"
  elAttr "meta"
    ( "name" =: "viewport"
   <> "content" =: T.intercalate ", " [ "width=device-width"
                                      , "initial-scale=1.0"
                                      , "maximum-scale=1.0"
                                      , "user-scalable=no"
                                      ]
    ) blank
  mapM_ (\h -> elAttr "link" ("rel" =: "stylesheet" <> "href" =: h) blank)
    [ -- "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/semantic.min.css"
      "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/reset.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/site.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/container.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/grid.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/header.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/image.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/menu.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/divider.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/segment.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/list.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/card.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/form.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/input.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/button.min.css"
    , "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/icon.min.css"
    ]
  CSS.inlineClay CSS.mainStylesheet
