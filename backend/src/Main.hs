module Main where

import           Control.Monad.Reader (runReaderT)

import           Backend.Client       (Client (..), clientApp)
import           Backend.Master       (withMaster, subscribe)
import           Backend.Server       (runServer)

-- | Create a "master" for handling global state, serve websocket app on port
-- 3000, which runs a "ClientApp" thread for each connection.
--
-- Note that, master is not a separate thread but simply a datatype that each
-- client has access to. This is more of a mental separation than anything else.
-- Without this notion, the client code would be directly modifying the
-- global state and issuing broadcasts to all the other clients. Instead of
-- dealing with that jumble, offloading to a master allows us to separate
-- concerns.
main :: IO ()
main =
  withMaster $ \master -> do
    runServer 3000 $ \conn -> do
      listener <- subscribe master
      runReaderT clientApp $ Client { _client_connection = conn
                                    , _client_listen = listener
                                    , _client_master = master
                                    }
