{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}
module Backend.Main
  ( main
  ) where

import           Control.Monad.IO.Class (MonadIO (..))
import           Data.Maybe             (fromMaybe)
import           System.Environment     (lookupEnv)
import           Text.Read              (readMaybe)

import           Control.Monad.Reader   (MonadReader, ReaderT)
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
    connHandler <- App.mkConnectionHandler db unAppT
    Server.runServer port connHandler
