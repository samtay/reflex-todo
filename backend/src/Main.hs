module Main where

import           Backend.App    (app)
import           Backend.Data   (openState)
import           Backend.Server (runServer)

-- TODO fix module weirdness, app should be passed to server
main :: IO ()
main = do
  state <- openState -- TODO does this need to be in IORef / MVar ???
  broadcast <- newBroadcastTChan
  runServer 3000 state broadcast
