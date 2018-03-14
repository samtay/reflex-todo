{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecursiveDo           #-}
module Frontend.Static (app) where

import           Data.Semigroup ((<>))

import           Data.Map       (Map)
import qualified Data.Map       as Map
import           Data.Map.Misc  (applyMap)
import           Reflex
import           Reflex.Dom

import           Common.Types
import           Frontend.Item

-- TODO add a permanent error message explaining non-persistent
app :: MonadWidget t m => m ()
app = do
  divClass "main" $ do
    -- Header
    elClass "h2" "ui center aligned icon header" $ do
      elClass "i" "circular list alternate outline icon" $ blank
      text "ToDo List"
    -- List items
    divClass "ui text container" $ divClass "ui segments" $ do
      rec items <- foldDyn ($) initItems $ leftmost [ mapSnoc . mkItem <$> newItem
                                                    , applyMap <$> updateUncompletes
                                                    , applyMap <$> updateCompletes
                                                    ]
          -- Uncompleted items
          updateUncompletes <- divClass "ui segment" $
            divClass "ui big ordered relaxed divided list" $ do
              listViewWithKey (Map.filter (not . _item_completed) <$> items) drawItem
          -- New item
          newItem <- drawItemInput
          -- Completed items
          updateCompletes <- divClass "ui segment" $
            divClass "ui big relaxed divided list" $ do
              listViewWithKey (Map.filter _item_completed <$> items) drawItem
      return ()
  -- Footer
  divClass "footer" $ divClass "ui center aligned container" $ do
    elAttr "a" ("href" =: "https://github.com/samtay/reflex-todo" <> "target" =: "_blank") $
      elAttr "button" ("class" =: "ui circular icon basic button") $ do
        elClass "i" "github icon" blank
  where
    mkItem t = Item t False
    initItems = Map.fromList $ zip [1..] $ fmap mkItem
      [ "Send app store links to the FP meetup"
      , "RSVP to the FP meetup"
      , "Shower"
      ]

mapSnoc :: a -> Map Int a -> Map Int a
mapSnoc a xs = case Map.toDescList xs of
                 (k, _) : _ -> Map.insert (succ k) a xs
                 _          -> Map.singleton 1 a

