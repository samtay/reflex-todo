{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Concurrent
import           Data.Either            (isLeft)
import           Data.Maybe             (catMaybes)

import           Control.Concurrent.STM
import           Control.Lens           ((^?))
import           Control.Monad.Reader
import qualified Data.Acid              as Acid
import qualified Data.Acid.Memory       as Acid
import           Data.Map               (Map, (!))
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Test.Hspec

import           Backend.App            (AppContext, HasConnection (..))
import qualified Backend.App            as App
import           Backend.Data           (TodoDb (..))
import           Common.Api
import           Common.Types

newtype TestAppT m a = TestAppT { unTestAppT :: ReaderT (AppContext MockConnection) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AppContext MockConnection))

instance MonadIO m => HasConnection (TestAppT m) where
  type Connection (TestAppT m) = MockConnection
  sendData (MockConnection hDown _ _) msg = liftIO . atomically $ writeTChan hDown msg
  receiveData (MockConnection _ hUp _) = liftIO . atomically $ readTChan hUp

data MockConnection = MockConnection
  { _mockConnection_handleDown :: TChan WebSocketDataDown
  , _mockConnection_handleUp   :: TChan (Either String WebSocketDataUp)
  , _mockConnection_threadId   :: ThreadId
  }

instance Show MockConnection where
  show (MockConnection _ _ tid) = "MockConnection: " ++ show tid

main :: IO ()
main = hspec $ do
  describe "interactivity" $ parallel $ before mockInteractivity $ do
    it "should generate consistent data" $ mapM_ $ \c ->
      resultingTodoList c `shouldReturn` expectedInteractivityTodoList
    it "should send appropriate success responses" $ \cs -> do
      resultingResponses (cs ! "createMock") `shouldReturn` replicate 3 (Right ())
      resultingResponses (cs ! "createThenErrMock") `shouldReturn` [Right ()]
      resultingResponses (cs ! "completeMock") `shouldReturn` [Right ()]
      resultingResponses (cs ! "deleteMock") `shouldReturn` [Right ()]
    it "should send appropriate error messages" $ \cs -> do
      resultingResponses (cs ! "completeErrMock") `shouldReturn` [Left errMessage]
  describe "correctness under load" $ parallel $ before mockLoad $ do
    it "should handle simultaneous creations" $ mapM_ . mapM_ $ \c ->
      resultingTodoList c `shouldReturn` expectedLoadTodoList
    it "should not care about duplicate completes" $ \cs ->
      countEithers . concat <$> mapM resultingResponses (cs ! "completeMocks")
        `shouldReturn` (0, 100)
    it "should reject duplicate deletes" $ \cs ->
      countEithers . concat <$> mapM resultingResponses (cs ! "deleteMocks")
        `shouldReturn` (99, 1)

-- Combine mock connections to demonstrate basic interactivity
mockInteractivity :: IO (Map Text MockConnection)
mockInteractivity = do
  db <- Acid.openMemoryState $ TodoDb mempty
  handler <- App.mkConnectionHandler db unTestAppT
  cs <- traverse (>>= \c -> forkIO (handler c) >> return c) conns
  threadDelay $ t 10
  Acid.closeAcidState db
  return cs
  where
    conns = Map.fromList $ [ ("createMock", createMock)
                           , ("createThenErrMock", createThenErrMock)
                           , ("completeMock", completeMock)
                           , ("deleteMock", deleteMock)
                           , ("completeErrMock", completeErrMock)
                           ]

-- Replicate mock connections to simulate a high load environment
mockLoad :: IO (Map Text [MockConnection])
mockLoad = do
  db <- Acid.openMemoryState $ TodoDb mempty
  handler <- App.mkConnectionHandler db unTestAppT
  cs <- forM conns $ traverse (>>= \c -> forkIO (handler c) >> return c)
  threadDelay $ t 10
  Acid.closeAcidState db
  return cs
  where
    conns = Map.fromList $ [ ("createMocks", replicate 100 createMock)
                           , ("completeMocks", replicate 100 completeMock)
                           , ("deleteMocks", replicate 100 deleteMock)
                           ]

-- | Creates items at 0.0, 0.2, 0.4s
createMock :: IO MockConnection
createMock = mkMock $ \_ hUp -> do
  replicateM_ 3 $ do
    atomically . writeTChan hUp .
      Right . WebSocketDataUp_Request $ TodoRequest_Create "createMock"
    threadDelay $ t 2

-- | Creates an item at 0.3s then sends a malformed request at 0.5s
createThenErrMock :: IO MockConnection
createThenErrMock = mkMock $ \_ hUp -> do
  threadDelay $ t 3
  atomically . writeTChan hUp .
    Right . WebSocketDataUp_Request $ TodoRequest_Create "createThenErrMock"
  threadDelay $ t 2
  atomically . writeTChan hUp .
    Left $ "Malformed request"

-- | Completes item 0 at 0.1s
completeMock :: IO MockConnection
completeMock = mkMock $ \_ hUp -> do
  threadDelay $ t 1
  atomically . writeTChan hUp .
    Right . WebSocketDataUp_Request $ TodoRequest_Complete 0

-- | Deletes item 1 at 0.5s
deleteMock :: IO MockConnection
deleteMock = mkMock $ \_ hUp -> do
  threadDelay $ t 5
  atomically . writeTChan hUp .
    Right . WebSocketDataUp_Request $ TodoRequest_Delete 1

-- | Tries to complete already deleted item at 0.9s
completeErrMock :: IO MockConnection
completeErrMock = mkMock $ \_ hUp -> do
  threadDelay $ t 9
  atomically . writeTChan hUp .
    Right . WebSocketDataUp_Request $ TodoRequest_Complete 1

mkMock
  :: (TChan WebSocketDataDown -> TChan (Either String WebSocketDataUp) -> IO ())
  -> IO MockConnection
mkMock handler = do
  hDown <- newTChanIO
  hUp <- newTChanIO
  tId <- forkIO $ handler hDown hUp
  return $ MockConnection hDown hUp tId

-- | The correct result of all of the mockInteractivity requests.
expectedInteractivityTodoList :: Map Int Item
expectedInteractivityTodoList = Map.fromList
  [ (0, Item "createMock" True)
  , (2, Item "createThenErrMock" False)
  , (3, Item "createMock" False)
  ]

-- | The correct result of all of the mockLoad requests.
expectedLoadTodoList :: Map Int Item
expectedLoadTodoList = Map.insert 0 (Item "createMock" True) $ Map.fromList
  [ (i, Item "createMock" False) | i <- [2 .. 3 * 100 - 1] ]

-- | This is the logic for the frontend watchers (see 'Frontend.App.app'). We
-- want to make sure that all frontend connections, no matter when they've
-- joined or what kind of requests they've sent, see the same list.
resultingTodoList :: MockConnection -> IO (Map Int Item)
resultingTodoList c = foldr ($) mempty . reverse <$> foldResponses mkMapModifier c
  where
    mkMapModifier = \case
      WebSocketDataDown_Listen (TodoListen_ListFull xs) -> Just $ const xs
      WebSocketDataDown_Listen (TodoListen_ListPatch p) -> Just $ applyMap p
      _                                                 -> Nothing

-- | Get all of the websocket listener messages for a given mock
resultingListens :: MockConnection -> IO [TodoListen]
resultingListens = foldResponses (^? _WebSocketDataDown_Listen)

-- | Get all of the websocket direct response messages for a given mock
resultingResponses :: MockConnection -> IO [TodoResponse]
resultingResponses = foldResponses (^? _WebSocketDataDown_Response)

foldResponses
  :: (WebSocketDataDown -> Maybe a)
  -> MockConnection
  -> IO [a]
foldResponses f c =
  let hDown = _mockConnection_handleDown c
      loop = tryReadTChan hDown >>= maybe (return []) (\a -> (f a :) <$> loop)
  in catMaybes <$> atomically loop

-- | TODO datatype + ShowPretty instance
errMessage :: Text
errMessage = "Oops, someone already deleted that item"

countEithers :: [Either a b] -> (Int, Int)
countEithers =
  let part (Left _) (l, r)  = (l + 1, r)
      part (Right _) (l, r) = (l, r + 1)
  in foldr part (0, 0)

-- | Duped from reflex because there is no 0.5 release available for stack...
applyMap :: Ord k => Map k (Maybe v) -> Map k v -> Map k v
applyMap patch old = insertions `Map.union` (old `Map.difference` deletions)
  where
    (deletions, insertions) = mapPartitionEithers $ maybeToEither <$> patch
    maybeToEither = \case
      Nothing -> Left ()
      Just r -> Right r

-- | Duped from reflex because there is no 0.5 release available for stack...
mapPartitionEithers :: Map k (Either a b) -> (Map k a, Map k b)
mapPartitionEithers m = (fromLeft <$> ls, fromRight <$> rs)
  where
    (ls, rs) = Map.partition isLeft m
    fromLeft (Left l) = l
    fromLeft _ = error "mapPartitionEithers: fromLeft received a Right value; this should be impossible"
    fromRight (Right r) = r
    fromRight _ = error "mapPartitionEithers: fromRight received a Left value; this should be impossible"

-- | Multiply microsends to make 0.1s units
t :: Int -> Int
t = (* 100000)
