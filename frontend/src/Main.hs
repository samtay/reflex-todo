{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Data.Semigroup                   ((<>))

import qualified Data.Text                        as T
import           Reflex.Dom
#ifdef MIN_VERSION_jsaddle_warp
import qualified Language.Javascript.JSaddle.Warp as JSaddle.Warp
import qualified Reflex.Dom.Core
#endif

import qualified Frontend.App                     as App
import qualified Frontend.CSS                     as CSS
import qualified Frontend.Static                  as Static

main :: IO ()
main = Reflex.Dom.mainWidgetWithHead headWidget appBody

appBody :: MonadWidget t m => m ()
appBody = do -- Start up ws connection, create dynamic, if conn drops use static
  let items = constDyn mempty
      connected = constDyn False -- TODO Create ConnectionStatus = 1 | 2 | 3
                                 -- Corresponding to connected, loading, failed (display static with err msg)
                                 -- holdUniqDyn obvi
  widgetHold_ (App.app items) $ ffor (updated connected) $ \case
    True -> App.app items
    False -> Static.app

#ifdef MIN_VERSION_jsaddle_warp
warp :: IO ()
warp = JSaddle.Warp.run 3911 $ Reflex.Dom.Core.mainWidgetWithHead headWidget appBody
#endif

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
  CSS.inlineClay CSS.mainStylesheet
  mapM_ includeSemantic [ "reset" , "site" , "container" , "grid"
                        , "header" , "image" , "menu" , "divider"
                        , "segment" , "list" , "card" , "form"
                        , "input" , "button" , "icon"
                        ]
  where
    includeSemantic component =
      let url = mconcat [ "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/"
                        , component
                        , ".min.css"
                        ]
      in elAttr "link" ("rel" =: "stylesheet" <> "href" =: url) blank
