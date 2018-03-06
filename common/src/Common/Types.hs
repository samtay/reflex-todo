{-# LANGUAGE DeriveGeneric #-}
module Common.Types where

import           Data.Aeson   (FromJSON (..), ToJSON (..))
import           Data.Text    (Text)
import           GHC.Generics (Generic)

data Item = Item
  { _item_text      :: Text
  , _item_completed :: Bool
  } deriving (Generic, Show)

instance ToJSON Item
instance FromJSON Item
