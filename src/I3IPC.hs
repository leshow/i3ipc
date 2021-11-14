-- |
-- Module:      I3IPC
-- Copyright:   (c) 2019 Evan Cameron
-- License:     BSD3
-- Maintainer:  Evan Cameron <cameron.evan@gmail.com>
--
-- Types and functions for interacting with i3's IPC mechanism
--


module I3IPC
    (
    -- ** Subscribe to events
    -- $sub

    -- ** Sending messages
    -- $msg

    -- ** Convenience functions
    -- $func    
      getSocketPath
    , getSwaySocketPath
    , Response(..)
    , subscribe
    , subscribeM
    , receive
    , receive'
    , receiveMsg
    , receiveMsg'
    , getReply
    , connecti3
    , connectsway
    , receiveEvent
    , receiveEvent'
    , runCommand
    , runCommand'
    , getWorkspaces
    , getWorkspaces'
    , getOutputs
    , getOutputs'
    , getTree
    , getTree'
    , getMarks
    , getMarks'
    , getVersion
    , getVersion'
    , getBarConfig
    , getBarConfig'
    , getBarIds
    , getBindingModes
    , getBindingModes'
    , getConfig
    , getConfig'
    , getTick
    , getTick'
    , getSync
    , getSync'
    ) where

import qualified I3IPC.Event                        as Evt
import qualified I3IPC.Message                      as Msg
import           I3IPC.Reply
import qualified I3IPC.Subscribe                    as Sub

import           Control.Exception                   ( Exception )
import           Control.Monad.Catch                 ( MonadThrow
                                                     , throwM
                                                     )
import           Control.Monad.IO.Class
import           Data.Maybe                          ( isJust )
import           Data.Semigroup                      ( (<>) )
import           System.Environment                  ( lookupEnv )
import           System.Exit                         ( ExitCode(..) )
import           System.Process.Typed                ( proc
                                                     , readProcess
                                                     )

import           Data.Aeson                          ( encode )
import           Data.Bifunctor                      ( second )
import           Data.Binary.Get                     ( getWord32le
                                                     , runGet
                                                     )
import           Data.Bits                           ( clearBit
                                                     , testBit
                                                     )
import qualified Data.ByteString.Lazy.Char8         as BL
import qualified Data.Text                          as T
import           Data.Typeable                       ( Typeable )
import           Network.Socket                      ( Family(AF_UNIX)
                                                     , SockAddr(SockAddrUnix)
                                                     , Socket
                                                     , SocketType(Stream)
                                                     , close
                                                     , connect
                                                     , socket
                                                     )
import           Network.Socket.ByteString.Lazy      ( recv )

-- | Exception type
data I3Exception = ConnectException T.Text | ProcessException
    deriving stock (Show, Eq, Typeable)

instance Exception I3Exception

-- | Get a new unix socket path from i3
getSocketPath :: MonadIO m => m (Maybe BL.ByteString)
getSocketPath = do
    res <- liftIO $ lookupEnv "I3SOCK"
    if isJust res
        then pure $ fmap BL.pack res
        else do
            (exitCode, out, _) <- readProcess $ proc "i3" ["--get-socketpath"]
            if exitCode /= ExitSuccess
                then pure Nothing
                else pure $ Just (BL.filter (/= '\n') out)

-- | Get a new unix socket path from sway
getSwaySocketPath :: MonadIO m => m (Maybe BL.ByteString)
getSwaySocketPath = fmap BL.pack <$> liftIO (lookupEnv "SWAYSOCK")

-- | Subscribe with a list of 'I3IPC.Subscribe.Subscribe' types, and subscribe will to respond with specific 'I3IPC.Event.Event'
subscribe
    :: (MonadThrow m, MonadIO m)
    => (Either String Evt.Event -> m ())
    -> [Sub.Subscribe]
    -> m ()
subscribe handle subtypes = do
    soc <- connecti3
    Msg.sendMsgPayload soc Msg.Subscribe (encode subtypes)
        >> receiveMsg soc
        >> handleSoc soc
        >> liftIO (close soc)
  where
    handleSoc soc = do
        r <- receiveEvent soc
        handle r
        handleSoc soc

-- | A version of 'subscribe' that allows the use of any monad transformer on top of MonadIO (kept around for backwards compatibility)
subscribeM
    :: (MonadThrow m, MonadIO m)
    => (Either String Evt.Event -> m ())
    -> [Sub.Subscribe]
    -> m ()
subscribeM handle subtypes = do
    soc <- connecti3
    Msg.sendMsgPayload soc Msg.Subscribe (encode subtypes)
        >> receiveMsg soc
        >> pure ()
    handleSoc soc >> liftIO (close soc)
  where
    handleSoc soc = do
        r <- receiveEvent soc
        handle r
        handleSoc soc

-- | Connect to an i3 socket and return it
connecti3 :: (MonadThrow m, MonadIO m) => m Socket
connecti3 = do
    soc <- liftIO $ socket AF_UNIX Stream 0
    getSocketPath >>= \case
        Nothing    -> throwM $ ConnectException "Failed to get i3 socket path"
        Just addr' -> do
            liftIO $ connect soc (SockAddrUnix $ BL.unpack addr')
            pure soc

-- | Connect to SWAY socket and return it
connectsway :: (MonadThrow m, MonadIO m) => m Socket
connectsway = do
    soc <- liftIO $ socket AF_UNIX Stream 0
    getSwaySocketPath >>= \case
        Nothing    -> throwM $ ConnectException "Failed to get i3 socket path"
        Just addr' -> do
            liftIO $ connect soc (SockAddrUnix $ BL.unpack addr')
            pure soc

-- | Useful for when you are receiving Events or Messages.
data Response = Message MsgReply | Event Evt.Event deriving (Show, Eq)

-- | Get and parse the response using i3's IPC
getReply :: MonadIO m => Socket -> m (Either String (Int, BL.ByteString))
getReply soc = do
    magic <- liftIO $ recv soc 6
    if magic == "i3-ipc"
        then do
            len  <- getInt <$> liftIO (recv soc 4)
            ty   <- getInt <$> liftIO (recv soc 4)
            body <- liftIO $ recv soc len
            pure $ Right (fromIntegral ty, body)
        else pure $ Left "Failed to get reply"
    where getInt = fromIntegral . runGet getWord32le

test :: Int -> BL.ByteString -> IO Int
test ty body = do
    putStrLn $ "type " <> show (ty `clearBit` 31)
    BL.putStrLn $ "body " <> body
    BL.putStrLn ""
    pure ty

-- | Parse response from socket, returning either an error or a 'I3IPC.Response', representing a sum type of a 'I3IPC.Reply.MsgReply' or 'I3IPC.Event.Event'
receive :: MonadIO m => Socket -> m (Either String Response)
receive soc = do
    reply <- getReply soc
    case reply of
        Right (ty, body) -> pure $ if testBit ty 31
            then Event `second` Evt.toEvent (ty `clearBit` 31) body
            else Message `second` toMsgReply ty body
        _ -> pure $ Left "Get Reply failed"

-- | Like receive but strict-- will use eitherDecode' under the hood to parse
receive' :: MonadIO m => Socket -> m (Either String Response)
receive' soc = do
    reply <- getReply soc
    case reply of
        Right (ty, body) -> pure $ if testBit ty 31
            then Event `second` Evt.toEvent' (ty `clearBit` 31) body
            else Message `second` toMsgReply' ty body
        _ -> pure $ Left "Get Reply failed"

-- | Receive but specifically for msgs, for when you know the response won't include any Events
receiveMsg :: MonadIO m => Socket -> m (Either String MsgReply)
receiveMsg soc = do
    r <- getReply soc
    pure $ do
        (ty, body) <- r
        toMsgReply ty body

-- | Like 'I3IPC.receiveMsg' but strict-- uses eitherDecode'
receiveMsg' :: MonadIO m => Socket -> m (Either String MsgReply)
receiveMsg' soc = do
    r <- getReply soc
    pure $ do
        (ty, body) <- r
        toMsgReply' ty body

-- | 'I3IPC.receive' specifically for Event
receiveEvent :: MonadIO m => Socket -> m (Either String Evt.Event)
receiveEvent soc = do
    r <- getReply soc
    pure $ do
        (ty, body) <- r
        Evt.toEvent (ty `clearBit` 31) body

-- | like 'receiveEvent' but strict-- uses eitherDecode'
receiveEvent' :: MonadIO m => Socket -> m (Either String Evt.Event)
receiveEvent' soc = do
    r <- getReply soc
    pure $ do
        (ty, body) <- r
        Evt.toEvent' (ty `clearBit` 31) body

-- | Run a command represented as a ByteString, all the following functions are convenience wrappers around
--
-- > Msg.sendMsgPayload soc Msg.X b >> receiveMsg soc
--
-- Or, if there is no message body:
--
-- > Msg.sendMsg soc Msg.X >> receiveMsg soc
runCommand
    :: MonadIO m => Socket -> BL.ByteString -> m (Either String MsgReply)
runCommand soc b = Msg.sendMsgPayload soc Msg.RunCommand b >> receiveMsg soc

runCommand'
    :: MonadIO m => Socket -> BL.ByteString -> m (Either String MsgReply)
runCommand' soc b = Msg.sendMsgPayload soc Msg.RunCommand b >> receiveMsg' soc

getWorkspaces :: MonadIO m => Socket -> m (Either String MsgReply)
getWorkspaces soc = Msg.sendMsg soc Msg.Workspaces >> receiveMsg soc

getWorkspaces' :: MonadIO m => Socket -> m (Either String MsgReply)
getWorkspaces' soc = Msg.sendMsg soc Msg.Workspaces >> receiveMsg' soc

getOutputs :: MonadIO m => Socket -> m (Either String MsgReply)
getOutputs soc = Msg.sendMsg soc Msg.Outputs >> receiveMsg soc

getOutputs' :: MonadIO m => Socket -> m (Either String MsgReply)
getOutputs' soc = Msg.sendMsg soc Msg.Outputs >> receiveMsg' soc

getTree :: MonadIO m => Socket -> m (Either String MsgReply)
getTree soc = Msg.sendMsg soc Msg.Tree >> receiveMsg soc

getTree' :: MonadIO m => Socket -> m (Either String MsgReply)
getTree' soc = Msg.sendMsg soc Msg.Tree >> receiveMsg' soc

getMarks :: MonadIO m => Socket -> m (Either String MsgReply)
getMarks soc = Msg.sendMsg soc Msg.Marks >> receiveMsg soc

getMarks' :: MonadIO m => Socket -> m (Either String MsgReply)
getMarks' soc = Msg.sendMsg soc Msg.Marks >> receiveMsg' soc

getBarIds :: MonadIO m => Socket -> m (Either String BarIds)
getBarIds soc = do
    _ <- Msg.sendMsg soc Msg.BarConfig
    r <- getReply soc
    pure $ do
        body <- r
        decodeBarIds (snd body)

-- | Get a bar's config based on it's id
getBarConfig
    :: MonadIO m => Socket -> BL.ByteString -> m (Either String MsgReply)
getBarConfig soc b = Msg.sendMsgPayload soc Msg.BarConfig b >> receiveMsg' soc

-- | Like 'I3IPC.getBarConfig' but strict
getBarConfig'
    :: MonadIO m => Socket -> BL.ByteString -> m (Either String MsgReply)
getBarConfig' soc b = Msg.sendMsgPayload soc Msg.BarConfig b >> receiveMsg' soc

getVersion :: MonadIO m => Socket -> m (Either String MsgReply)
getVersion soc = Msg.sendMsg soc Msg.Version >> receiveMsg soc

getVersion' :: MonadIO m => Socket -> m (Either String MsgReply)
getVersion' soc = Msg.sendMsg soc Msg.Version >> receiveMsg' soc

getBindingModes :: MonadIO m => Socket -> m (Either String MsgReply)
getBindingModes soc = Msg.sendMsg soc Msg.BindingModes >> receiveMsg soc

getBindingModes' :: MonadIO m => Socket -> m (Either String MsgReply)
getBindingModes' soc = Msg.sendMsg soc Msg.BindingModes >> receiveMsg' soc

getConfig :: MonadIO m => Socket -> m (Either String MsgReply)
getConfig soc = Msg.sendMsg soc Msg.Config >> receiveMsg soc

getConfig' :: MonadIO m => Socket -> m (Either String MsgReply)
getConfig' soc = Msg.sendMsg soc Msg.Config >> receiveMsg' soc

getTick :: MonadIO m => Socket -> m (Either String MsgReply)
getTick soc = Msg.sendMsg soc Msg.Tick >> receiveMsg soc

getTick' :: MonadIO m => Socket -> m (Either String MsgReply)
getTick' soc = Msg.sendMsg soc Msg.Tick >> receiveMsg' soc

getSync :: MonadIO m => Socket -> m (Either String MsgReply)
getSync soc = Msg.sendMsg soc Msg.Sync >> receiveMsg soc

getSync' :: MonadIO m => Socket -> m (Either String MsgReply)
getSync' soc = Msg.sendMsg soc Msg.Sync >> receiveMsg' soc



-- $sub
--
-- Commonly, you just want to subscribe to a set of event types and do something with the response:
--
-- > import qualified I3IPC.Subscribe               as Sub
-- > import           I3IPC.Event
-- > import           I3IPC                          ( subscribe )
-- > import           Control.Monad.IO.Class 
-- > 
-- > main :: IO ()
-- > main = liftIO $ subscribe handle [Sub.Workspace, Sub.Window]
-- >  where
-- >   handle :: Either String Event -> IO ()
-- >   handle (Right evt) = case evt of
-- >     Workspace WorkspaceEvent { wrk_current } -> print wrk_current
-- >     Window WindowEvent { win_container } -> print win_container
-- >     _ -> error "No other event types"
-- >   handle (Left err) = error err
--

-- $msg
--
-- Other times, you want to send some kind of command to i3, or get a specific response as a one-time action.
--
-- > import           I3IPC              ( connecti3
-- >                                     , getWorkspaces
-- >                                     )
-- > import           Control.Monad.IO.Class 
-- > 
-- > main :: IO ()
-- > main = do
-- >     soc <- liftIO $ connecti3
-- >     print getWorkspaces
-- 
-- $func
--
-- All of the "getX" functions are provided for convenience, but also exported are the building blocks to write whatever you like.
-- There are strict and non-strict variants provided, the tick (') implies strict.
-- For instance, the above could be written as:
--
-- > import qualified I3IPC.Message     as Msg
-- > import           I3IPC              ( connecti3
-- >                                     , receiveMsg
-- >                                     )
-- > import           Control.Monad.IO.Class 
-- > 
-- > main :: IO ()
-- > main = do
-- >     soc <- liftIO $ connecti3
-- >     print $ Msg.sendMsg soc Msg.Workspaces >> receiveMsg soc
