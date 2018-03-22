{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Data.Semigroup                   ((<>))

import           Control.Monad.Reader             (asks)
import qualified Data.Map                         as Map
import           Reflex.Dom                       hiding (link)
#ifdef MIN_VERSION_jsaddle_warp
import qualified Language.Javascript.JSaddle.Warp as JSaddle.Warp
import qualified Reflex.Dom.Core
#endif

import qualified Frontend.App                     as App
import qualified Frontend.CSS                     as CSS
import           Frontend.Env
import qualified Frontend.Static                  as Static

main :: IO ()
main = Reflex.Dom.mainWidgetWithHead headWidget appBody
-- TODO away with this, use Reflex.Dom.Internal.run which already handles per-platform startup
#ifdef MIN_VERSION_jsaddle_warp
warp :: IO ()
warp = JSaddle.Warp.run 3911 $ Reflex.Dom.Core.mainWidgetWithHead headWidget appBody
#endif

appBody :: MonadWidget t m => m ()
appBody = runApp $ do
  request <- withListHeader $ do
    App.messageWidget
    connection <- asks _todoEnv_connection
    widgetHold App.app $ ffor (updated connection) $ \case
      Connection_Disconnected -> Static.app >> return never
      _                       -> App.app
  footer
  return . switch . current $ request

headWidget :: MonadWidget t m => m ()
headWidget = do
  el "title" $ text "Reflex ToDo"
  meta [ ("name", "viewport")
       , ("content", "width=device-width,initial-scale=1.0,maximum-scale=1.0,user-scalable=no")
       ]
  link [ ("rel", "apple-touch-icon")
       , ("sizes", "180x180")
       , ("href", "/apple-touch-icon.png")
       ]
  link [ ("rel", "icon")
       , ("sizes", "32x32")
       , ("type", "image/png")
       , ("href", "/favicon-32x32.png")
       ]
  link [ ("rel", "icon")
       , ("sizes", "16x16")
       , ("type", "image/png")
       , ("href", "/favicon-16x16.png")
       ]
  link [ ("rel", "manifest")
       , ("href", "/site.webmanifest")
       ]
  link [ ("rel", "mask-icon")
       , ("href", "/safari-pinned-tab.svg")
       , ("color", "#00b5ad")
       ]
  meta [ ("name", "theme-color")
       , ("content", "#00b5ad")
       , ("color", "#00b5ad")
       ]
  CSS.inlineClay CSS.mainStylesheet
  mapM_ includeSemantic [ "reset" , "site" , "container" , "grid"
                        , "header" , "image" , "menu" , "divider"
                        , "segment" , "list" , "card" , "form"
                        , "input" , "button" , "icon", "message"
                        ]
  where
    link = blankElAttr "link"
    meta = blankElAttr "meta"
    blankElAttr n attrs = elAttr n (Map.fromList attrs) blank
    includeSemantic component =
      let url = mconcat [ "https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.3.0/components/"
                        , component
                        , ".min.css"
                        ]
      in link [("rel", "stylesheet"), ("href", url)]

withListHeader :: MonadWidget t m => m a -> m a
withListHeader listWidget =
  divClass "main" $ do
    elClass "h2" "ui center aligned icon header" $ do
      elClass "i" "circular list alternate outline icon" $ blank
      text "ToDo List"
    listWidget

footer :: MonadWidget t m => m ()
footer =
  divClass "footer" $ divClass "ui center aligned container" $
    elAttr "a" ("href" =: "https://github.com/samtay/reflex-todo" <> "target" =: "_blank") $
      elAttr "button" ("class" =: "ui circular icon basic button") $
        elClass "i" "github icon" blank
