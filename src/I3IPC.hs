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
    , Response(..)
    , subscribe
    , receive
    , receive'
    , receiveMsg
    , receiveMsg'
    , getReply
    , connecti3
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
    )
where

import qualified I3IPC.Message                      as Msg
import qualified I3IPC.Subscribe                    as Sub
import qualified I3IPC.Event                        as Evt
import           I3IPC.Reply

import           Control.Monad                       ( unless
                                                     , forever
                                                     )
import           System.Environment                  ( lookupEnv )
import           Data.Either                         ( isLeft )
import           Data.Maybe                          ( isJust )
import           Data.Semigroup                      ( (<>) )
import           System.Process.Typed                ( proc
                                                     , readProcess
                                                     )
import           System.Exit                         ( ExitCode(..)
                                                     , exitFailure
                                                     )
import           Network.Socket               hiding ( send
                                                     , sendTo
                                                     , recv
                                                     , recvFrom
                                                     )
import           Network.Socket.ByteString.Lazy
import           Data.Aeson                          ( encode )
import           Data.Binary.Get
import           Data.Bifunctor                      ( second )
import qualified Data.ByteString.Lazy.Char8         as BL
import           Data.Bits                           ( testBit
                                                     , clearBit
                                                     )
import           Pipes

-- | Get a new unix socket path from i3
getSocketPath :: IO (Maybe BL.ByteString)
getSocketPath = do
    res <- lookupEnv "I3SOCK"
    if isJust res
        then pure $ fmap BL.pack res
        else do
            (exitCode, out, _) <- readProcess $ proc "i3" ["--get-socketpath"]
            if exitCode /= ExitSuccess
                then pure Nothing
                else pure $ Just (BL.filter (/= '\n') out)


-- | Subscribe with a list of 'I3IPC.Subscribe.Subscribe' types, and subscribe will to respond with specific 'I3IPC.Event.Event'
subscribe :: (Either String Evt.Event -> IO ()) -> [Sub.Subscribe] -> IO ()
subscribe handle subtypes = do
    soc <- connecti3
    Msg.sendMsgPayload soc Msg.Subscribe (encode subtypes)
        >> receiveMsg soc
        >> handleSoc soc
        >> close soc
  where
    handleSoc soc = do
        r <- receiveEvent soc
        handle r
        handleSoc soc

-- | Connect to an i3 socket and return it
connecti3 :: IO Socket
connecti3 = do
    soc <- socket AF_UNIX Stream 0
    getSocketPath >>= \case
        Nothing   -> putStrLn "Failed to get i3 socket path" >> exitFailure
        Just addr -> do
            connect soc (SockAddrUnix $ BL.unpack addr)
            pure soc

-- | Useful for when you are receiving Events or Messages.
data Response = Message MsgReply | Event Evt.Event deriving (Show, Eq)

-- | Get and parse the response using i3's IPC
getReply :: Socket -> IO (Either String (Int, BL.ByteString))
getReply soc = do
    magic <- recv soc 6
    if magic == "i3-ipc"
        then do
            len  <- fromIntegral . runGet getWord32le <$> recv soc 4
            ty   <- fromIntegral . runGet getWord32le <$> recv soc 4
            body <- recv soc len
            pure $ Right (ty, body)
        else pure $ Left "Failed to get reply"

test :: Int -> BL.ByteString -> IO Int
test ty body = do
    putStrLn $ "type " <> show (ty `clearBit` 31)
    BL.putStrLn $ "body " <> body
    BL.putStrLn ""
    pure ty

-- | Parse response from socket, returning either an error or a 'I3IPC.Response', representing a sum type of a 'I3IPC.Reply.MsgReply' or 'I3IPC.Event.Event'
receive :: Socket -> IO (Either String Response)
receive soc = do
    reply <- getReply soc
    case reply of
        Right (ty, body) -> pure $ if testBit ty 31
            then Event `second` Evt.toEvent (ty `clearBit` 31) body
            else Message `second` toMsgReply ty body
        _ -> pure $ Left "Get Reply failed"

-- | Like receive but strict-- will use eitherDecode' under the hood to parse
receive' :: Socket -> IO (Either String Response)
receive' soc = do
    reply <- getReply soc
    case reply of
        Right (ty, body) -> pure $ if testBit ty 31
            then Event `second` Evt.toEvent' (ty `clearBit` 31) body
            else Message `second` toMsgReply' ty body
        _ -> pure $ Left "Get Reply failed"

-- | Receive but specifically for msgs, for when you know the response won't include any Events
receiveMsg :: Socket -> IO (Either String MsgReply)
receiveMsg soc = do
    r <- getReply soc
    pure $ r >>= uncurry toMsgReply

-- | Like 'I3IPC.receiveMsg' but strict-- uses eitherDecode'
receiveMsg' :: Socket -> IO (Either String MsgReply)
receiveMsg' soc = do
    r <- getReply soc
    pure $ r >>= uncurry toMsgReply'

-- | 'I3IPC.receive' specifically for Event
receiveEvent :: Socket -> IO (Either String Evt.Event)
receiveEvent soc = do
    r <- getReply soc
    pure $ do
        (ty, body) <- r
        Evt.toEvent (ty `clearBit` 31) body

-- | like 'receiveEvent' but strict-- uses eitherDecode'
receiveEvent' :: Socket -> IO (Either String Evt.Event)
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
runCommand :: Socket -> BL.ByteString -> IO (Either String MsgReply)
runCommand soc b = Msg.sendMsgPayload soc Msg.RunCommand b >> receiveMsg soc

runCommand' :: Socket -> BL.ByteString -> IO (Either String MsgReply)
runCommand' soc b = Msg.sendMsgPayload soc Msg.RunCommand b >> receiveMsg' soc

getWorkspaces :: Socket -> IO (Either String MsgReply)
getWorkspaces soc = Msg.sendMsg soc Msg.Workspaces >> receiveMsg soc

getWorkspaces' :: Socket -> IO (Either String MsgReply)
getWorkspaces' soc = Msg.sendMsg soc Msg.Workspaces >> receiveMsg' soc

getOutputs :: Socket -> IO (Either String MsgReply)
getOutputs soc = Msg.sendMsg soc Msg.Outputs >> receiveMsg soc

getOutputs' :: Socket -> IO (Either String MsgReply)
getOutputs' soc = Msg.sendMsg soc Msg.Outputs >> receiveMsg' soc

getTree :: Socket -> IO (Either String MsgReply)
getTree soc = Msg.sendMsg soc Msg.Tree >> receiveMsg soc

getTree' :: Socket -> IO (Either String MsgReply)
getTree' soc = Msg.sendMsg soc Msg.Tree >> receiveMsg' soc

getMarks :: Socket -> IO (Either String MsgReply)
getMarks soc = Msg.sendMsg soc Msg.Marks >> receiveMsg soc

getMarks' :: Socket -> IO (Either String MsgReply)
getMarks' soc = Msg.sendMsg soc Msg.Marks >> receiveMsg' soc

getBarIds :: Socket -> IO (Either String BarIds)
getBarIds soc = do
    _ <- Msg.sendMsg soc Msg.BarConfig
    r <- getReply soc
    pure $ do
        body <- r
        decodeBarIds (snd body)

-- | Get a bar's config based on it's id
getBarConfig :: Socket -> BL.ByteString -> IO (Either String MsgReply)
getBarConfig soc b = Msg.sendMsgPayload soc Msg.BarConfig b >> receiveMsg' soc

-- | Like 'I3IPC.getBarConfig' but strict
getBarConfig' :: Socket -> BL.ByteString -> IO (Either String MsgReply)
getBarConfig' soc b = Msg.sendMsgPayload soc Msg.BarConfig b >> receiveMsg' soc

getVersion :: Socket -> IO (Either String MsgReply)
getVersion soc = Msg.sendMsg soc Msg.Version >> receiveMsg soc

getVersion' :: Socket -> IO (Either String MsgReply)
getVersion' soc = Msg.sendMsg soc Msg.Version >> receiveMsg' soc

getBindingModes :: Socket -> IO (Either String MsgReply)
getBindingModes soc = Msg.sendMsg soc Msg.BindingModes >> receiveMsg soc

getBindingModes' :: Socket -> IO (Either String MsgReply)
getBindingModes' soc = Msg.sendMsg soc Msg.BindingModes >> receiveMsg' soc

getConfig :: Socket -> IO (Either String MsgReply)
getConfig soc = Msg.sendMsg soc Msg.Config >> receiveMsg soc

getConfig' :: Socket -> IO (Either String MsgReply)
getConfig' soc = Msg.sendMsg soc Msg.Config >> receiveMsg' soc

getTick :: Socket -> IO (Either String MsgReply)
getTick soc = Msg.sendMsg soc Msg.Tick >> receiveMsg soc

getTick' :: Socket -> IO (Either String MsgReply)
getTick' soc = Msg.sendMsg soc Msg.Tick >> receiveMsg' soc

getSync :: Socket -> IO (Either String MsgReply)
getSync soc = Msg.sendMsg soc Msg.Sync >> receiveMsg soc

getSync' :: Socket -> IO (Either String MsgReply)
getSync' soc = Msg.sendMsg soc Msg.Sync >> receiveMsg' soc


-- | Connect to an i3 socket and return it
listenPipe
    :: MonadIO m => [Sub.Subscribe] -> Producer (Either String Evt.Event) m ()
listenPipe subtypes = do
    soc <- liftIO connecti3
    liftIO $ Msg.sendMsgPayload soc Msg.Subscribe (encode subtypes)
    liftIO $ receiveMsg soc
    listeni3Pipe soc 

closeSoc :: MonadIO m => Socket -> Consumer () m ()
closeSoc soc = liftIO $ close soc

-- >> handleSoc soc
-- >> close soc

listeni3Pipe :: MonadIO m => Socket -> Producer (Either String Evt.Event) m ()
listeni3Pipe soc = do
    r <- liftIO $ receiveEvent soc
    unless (isLeft r) $ do
        yield r
        listeni3Pipe soc

-- $sub
--
-- Commonly, you just want to subscribe to a set of event types and do something with the response:
--
-- > import qualified I3IPC.Subscribe               as Sub
-- > import           I3IPC.Event
-- > import           I3IPC                          ( subscribe )
-- > 
-- > main :: IO ()
-- > main = subscribe handle [Sub.Workspace, Sub.Window]
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
-- > 
-- > main :: IO ()
-- > main = do
-- >     soc <- connecti3
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
-- > 
-- > main :: IO ()
-- > main = do
-- >     soc <- connecti3
-- >     print $ Msg.sendMsg soc Msg.Workspaces >> receiveMsg soc
