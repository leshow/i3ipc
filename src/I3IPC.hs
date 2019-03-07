module I3IPC
    ( getSocketPath
    , subscribe
    , receive
    , receiveMsg
    , getReply
    , connecti3
    , receiveEvent
    , runCommand
    , getWorkspaces
    )
where

import qualified I3IPC.Message                      as Msg
import qualified I3IPC.Subscribe                    as Sub
import qualified I3IPC.Event                        as Evt
import           I3IPC.Reply

import           System.Environment                  ( lookupEnv )
import           Data.Maybe                          ( isJust )
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
import qualified Data.ByteString.Lazy.Char8         as BL
import           Data.Bits                           ( testBit
                                                     , clearBit
                                                     )

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


-- | Subscribe to i3 msgs of the specific 'ReplyType'
--
subscribe :: (Maybe Reply -> IO ()) -> [Sub.Subscribe] -> IO ()
subscribe handle subtypes = do
    soc  <- socket AF_UNIX Stream 0
    addr <- getSocketPath
    case addr of
        Nothing -> putStrLn "Failed to get i3 socket path" >> exitFailure
        Just addr' ->
            connect soc (SockAddrUnix $ BL.unpack addr')
                >> Msg.sendMsg soc Msg.Subscribe (encode subtypes)
                >> handleSoc soc
                >> close soc
    where handleSoc soc = do
            r <- receive soc
            handle r
            handleSoc soc


data Reply  = Message MsgReply | Event Evt.Event deriving (Show, Eq)

getReply :: Socket -> IO (Maybe (Int, BL.ByteString))
getReply soc = do
    magic <- recv soc 6
    if magic == "i3-ipc"
        then do
            len  <- fromIntegral . runGet getWord32le <$> recv soc 4
            ty   <- fromIntegral . runGet getWord32le <$> recv soc 4
            body <- recv soc len
            pure $ Just (ty, body)
        else pure Nothing

receive :: Socket -> IO (Maybe Reply)
receive soc = do
    reply <- getReply soc
    case reply of
        Just (ty, body) -> pure $ if testBit ty 31
            then Event <$> Evt.toEvent (ty `clearBit` 31) body
            else Message <$> toMsgReply ty body
        _ -> pure Nothing

receiveMsg :: Socket -> IO (Maybe MsgReply)
receiveMsg soc = do
    r <- getReply soc
    pure $ do
        (ty, body) <- r
        toMsgReply ty body

receiveEvent :: Socket -> IO (Maybe Evt.Event)
receiveEvent soc = do
    r <- getReply soc
    pure $ do
        (ty, body) <- r
        if testBit ty 31 then Evt.toEvent (ty `clearBit` 31) body else Nothing

connecti3 :: IO Socket
connecti3 = do
    soc  <- socket AF_UNIX Stream 0
    addr <- getSocketPath
    case addr of
        Nothing    -> putStrLn "Failed to get i3 socket path" >> exitFailure
        Just addr' -> do
            connect soc (SockAddrUnix $ BL.unpack addr')
            pure soc

runCommand :: Socket -> BL.ByteString -> IO (Maybe Reply)
runCommand soc b = Msg.sendMsg soc Msg.RunCommand b >> receive soc


getWorkspaces :: Socket -> IO (Maybe Reply)
getWorkspaces soc = Msg.sendMsg' soc Msg.Workspaces >> receive soc
