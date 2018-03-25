{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Backend.Main
  ( main
  ) where

import           Control.Exception      (finally)
import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Maybe             (fromMaybe)
import           System.Environment     (lookupEnv)
import           Text.Read              (readMaybe)

import           Control.Concurrent.STM (newTVarIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import           Data.Aeson             (eitherDecode, encode)
import qualified Network.WebSockets     as WS

import           Backend.App            (AppContext (..), HasConnection (..))
import qualified Backend.App            as App
import qualified Backend.Data           as Data
import qualified Backend.Server         as Server

newtype AppT m a = AppT { unAppT :: ReaderT (AppContext WS.Connection) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AppContext WS.Connection))

instance MonadIO m => HasConnection (AppT m) where
  type Connection (AppT m) = WS.Connection
  sendData conn msg = liftIO $ WS.sendTextData conn $ encode msg
  receiveData conn = liftIO $ eitherDecode <$> WS.receiveData conn

main :: IO ()
main = do
  port <- fromMaybe 3000 <$> (readMaybe =<<) <$> lookupEnv "PORT"
  Data.withTodoDb $ \db -> do
    clients <- newTVarIO mempty
    Server.runServer port $ \conn -> do
      cid <- App.connect clients conn
      finally
        (runReaderT (unAppT App.app) $ AppContext cid conn db clients)
        (App.disconnect clients cid)
