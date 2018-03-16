module Main where

import           Control.Exception      (bracket)

import           Control.Concurrent.STM (atomically, newBroadcastTChan)
import qualified Data.Acid.Local        as Acid

import           Backend.Data           (TodoDb (..))
import           Backend.Server         (runServer)

-- TODO fix module weirdness, app should be passed to server
  -- TODO does state need to be in IORef / MVar ???
main :: IO ()
main = do
  broadcast <- atomically newBroadcastTChan
  bracket
    (Acid.openLocalState (TodoDb mempty))
    Acid.createCheckpointAndClose
    (runServer 3000 broadcast)
