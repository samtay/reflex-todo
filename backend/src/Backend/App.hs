module Backend.App (app) where

import           Data.Acid
import qualified Network.WebSockets as WS

import           Backend.Data
import           Backend.Server

app
  :: AcidState TodoDb -- ^ Todo database
  -> NetworkState -- ^ Reference to all open WS connections
  -> Int -- ^ This connection client ID
  -> WS.Connection -- ^ This connection
  -> IO ()
app state conns clientId conn = undefined
