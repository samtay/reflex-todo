module Backend.App where

import           Control.Concurrent.STM.TChan
import           Control.Monad.Reader
import           Data.Acid
import qualified Network.WebSockets           as WS

import           Backend.Data
import           Common.Request               (TodoListen)

type App = ReaderT AppState IO

data AppState = AppState
  { _appState_connection :: WS.Connection
  , _appState_todoDb     :: AcidState TodoDb
  , _appState_broadcast  :: TChan TodoListen -- ^ Send updates
  , _appState_listen     :: TChan TodoListen -- ^ Receive updates
  }

app :: App ()
app = undefined
  -- Listen for requests from this client
  -- Listen for state updates from other clients
