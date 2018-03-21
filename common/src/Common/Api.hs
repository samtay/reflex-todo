{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE LambdaCase    #-}
module Common.Api where

import           GHC.Generics           (Generic)

import           Control.Lens           (Prism', prism)
import           Data.Aeson
import           Data.Map               (Map)
import           Data.Text              (Text)

import           Common.Types

{-
Note that separating Response / Listen data is pretty unnecessary since
Requester class is not being used, which allows the Response value events to be
created directly from the request call. With this codebase, there really is no
difference between the two, other than the fact that events we "listen" to are
sent to all clients, and "response"s are sent to specific clients.

I'd like to set this up to use runRequesterT, which is a very pleasant
typeclass to work with once set up, but I can't figure out how I'm supposed to
construct the RequesterData values that 'runRequesterT' requires.
-}

-- | Requests made from the frontend and consumed on the backend
data TodoRequest
  = TodoRequest_Create Text
  | TodoRequest_Complete Int
  | TodoRequest_Delete Int
  deriving (Generic)

-- | Responses to our requests. Currently just error text or silent success
type TodoResponse = Either Text ()

-- | This describes what we listen for, i.e. unsolicited notifications
-- from the server that don't start from a request like 'TodoRequest'
data TodoListen
  = TodoListen_ListFull (Map Int Item)          -- ^ Full list is passed on initial connection
  | TodoListen_ListPatch (Map Int (Maybe Item)) -- ^ Patches are sent to an active connection
  deriving (Generic)

-- | The data that goes up the websocket (client -> server)
data WebSocketDataUp
  = WebSocketDataUp_Request TodoRequest
  deriving (Generic)

-- | The data that goes up the websocket (client -> server)
data WebSocketDataDown
  = WebSocketDataDown_Listen TodoListen
  | WebSocketDataDown_Response TodoResponse
  deriving (Generic)

instance ToJSON TodoRequest
instance FromJSON TodoRequest
instance ToJSON TodoListen
instance FromJSON TodoListen
instance ToJSON WebSocketDataUp
instance FromJSON WebSocketDataUp
instance ToJSON WebSocketDataDown
instance FromJSON WebSocketDataDown

-- | Some manual lenses
_WebSocketDataDown_Listen :: Prism' WebSocketDataDown TodoListen
_WebSocketDataDown_Listen = prism WebSocketDataDown_Listen $ \case
  WebSocketDataDown_Listen l -> Right l
  x                          -> Left x
_WebSocketDataDown_Response :: Prism' WebSocketDataDown TodoResponse
_WebSocketDataDown_Response = prism WebSocketDataDown_Response $ \case
  WebSocketDataDown_Response r -> Right r
  x                            -> Left x
