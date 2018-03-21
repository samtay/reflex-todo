module Main where

import           Control.Monad.Reader (runReaderT)
import           Data.Maybe           (fromMaybe)
import           System.Environment   (lookupEnv)
import           Text.Read            (readMaybe)

import           Backend.Client       (Client (..), clientApp)
import           Backend.Master       (subscribe, withMaster)
import           Backend.Server       (runServer)

-- | Create a "master" for handling global state and serve websocket app on
-- port 3000, which runs a "ClientApp" thread for each connection.
--
-- Note that, master is not a separate thread for communicating with each
-- client, but simply a datatype that each client has access to. This is more
-- of a mental separation than anything else.  Without this notion, the client
-- code would be directly modifying the global state and issuing broadcasts to
-- all the other clients. Instead of dealing with that jumble, offloading to a
-- master allows us to separate concerns.
main :: IO ()
main = do
  port <- fromMaybe 3000 <$> (readMaybe =<<) <$> lookupEnv "PORT"
  withMaster $ \master -> do
    runServer port $ \conn -> do
      listener <- subscribe master
      runReaderT clientApp $ Client { _client_connection = conn
                                    , _client_listen = listener
                                    , _client_master = master
                                    }
