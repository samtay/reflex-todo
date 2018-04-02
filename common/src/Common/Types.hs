{-# LANGUAGE DeriveGeneric #-}
module Common.Types where

import           GHC.Generics (Generic)

import           Data.Aeson   (FromJSON (..), ToJSON (..))
import           Data.Text    (Text)

data Item = Item
  { _item_text      :: Text
  , _item_completed :: Bool
  } deriving (Eq, Generic, Show)

instance ToJSON Item
instance FromJSON Item
