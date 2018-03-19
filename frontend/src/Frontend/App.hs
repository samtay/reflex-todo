{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
module Frontend.App
  ( app
  , messageWidget
  ) where

import           Control.Monad        (void)
import           Control.Monad.Reader (ask, asks)
import           Data.Semigroup       ((<>))

import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           Reflex
import           Reflex.Dom

import           Common.Api
import           Common.Types
import           Frontend.Env
import           Frontend.Item

-- TODO why do I have to click complete twice ???
app :: TodoWidget t m (Event t [TodoRequest])
app = divClass "ui text container" $ divClass "ui segments" $ do
  items <- watchTodoItems
  -- Uncompleted items
  completes <- divClass "ui segment" $
    divClass "ui big ordered relaxed divided list" $ do
      listViewWithKey (Map.filter (not . _item_completed) <$> items) drawItem
  -- New item
  newItem <- drawItemInput
  -- Completed items
  deletes <- divClass "ui segment" $
    divClass "ui big relaxed divided list" $ do
      listViewWithKey (Map.filter _item_completed <$> items) drawItem
  -- Return request events
  return $ mergeWith (++) [ (:[]) . TodoRequest_Create <$> newItem
                          , fmap TodoRequest_Complete <$> Map.keys <$> completes
                          , fmap TodoRequest_Delete <$> Map.keys <$> deletes
                          ]

-- TODO why are double events getting sent from server ???
watchTodoItems :: TodoWidget t m (Dynamic t (Map Int Item))
watchTodoItems = do
  listUpdates <- asks _todoEnv_listen
  foldDyn ($) mempty . ffor listUpdates $ \case
    TodoListen_ListFull l  -> const l
    TodoListen_ListPatch p -> applyMap p

-- | Display API response text and connection notices
messageWidget :: TodoWidget t m ()
messageWidget = divClass "ui text container messages" $ do
  env <- ask
  void . dyn . ffor (_todoEnv_connection env) $ \case
    Connection_Disconnected -> disconnectedNotice
    Connection_Reconnecting -> reconnectingNotice
    _                       -> blank
  rec close <- widgetHold (return never) $ leftmost
        [ dismissableNotice "error" <$> filterLeft (_todoEnv_response env)
        , return never <$ switch (current close)
        ]
  return ()
  where
    disconnectedNotice = nondismissableNotice "warning" "exclamation triangle"
      "Websocket connection failed."
      "Your data will not persist but you can still play around."
    reconnectingNotice = nondismissableNotice "" "notched circle loading"
      "Reconnecting"
      "Please wait a minute..."

nondismissableNotice
  :: MonadWidget t m
  => Text -- ^ Message class
  -> Text -- ^ Icon class
  -> Text -- ^ Header text
  -> Text -- ^ Message text
  -> m ()
nondismissableNotice cls icon header msg =
  divClass ("ui icon message " <> cls) $ do
    elClass "i" (icon <> " icon") blank
    divClass "content" $ do
      divClass "header" $ text header
      el "p" $ text msg

dismissableNotice
  :: MonadWidget t m
  => Text -- ^ Message class
  -> Text -- ^ Message text
  -> m (Event t ())
dismissableNotice cls msg =
  divClass ("ui message " <> cls) $ do
    close <- fmap (domEvent Click . fst) $ elClass' "i" "close icon" blank
    text msg
    return close
