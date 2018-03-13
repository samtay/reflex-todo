{-# LANGUAGE CPP                   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}

module Main where

import           Data.Semigroup                   ((<>))

import           Control.Monad.Reader             (asks)
import qualified Data.Text                        as T
import           Reflex.Dom
#ifdef MIN_VERSION_jsaddle_warp
import qualified Language.Javascript.JSaddle.Warp as JSaddle.Warp
import qualified Reflex.Dom.Core
#endif

import qualified Frontend.App                     as App
import qualified Frontend.CSS                     as CSS
import qualified Frontend.Static                  as Static
import           Frontend.Env

main :: IO ()
main = Reflex.Dom.mainWidgetWithHead headWidget appBody
-- TODO away with this, use Reflex.Dom.Internal.run which already handles per-platform startup
#ifdef MIN_VERSION_jsaddle_warp
warp :: IO ()
warp = JSaddle.Warp.run 3911 $ Reflex.Dom.Core.mainWidgetWithHead headWidget appBody
#endif

appBody :: MonadWidget t m => m ()
appBody = runApp $ do
  connection <- asks _todoEnv_connection
  requestDynEvt <- widgetHold App.app $ ffor (updated connection) $ \case
    Connection_Disconnected -> Static.app >> return never
    _                       -> App.app
  return . switch . current $ requestDynEvt


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
