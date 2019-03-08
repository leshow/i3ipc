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
import           Data.Bifunctor                      ( second )
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
subscribe :: (Either String Evt.Event -> IO ()) -> [Sub.Subscribe] -> IO ()
subscribe handle subtypes = do
    soc  <- socket AF_UNIX Stream 0
    addr <- getSocketPath
    case addr of
        Nothing -> putStrLn "Failed to get i3 socket path" >> exitFailure
        Just addr' ->
            connect soc (SockAddrUnix $ BL.unpack addr')
                >> Msg.sendMsg soc Msg.Subscribe (encode subtypes)
                >> receiveMsg soc
                >> handleSoc soc
                >> close soc
  where
    handleSoc soc = do
        r <- receiveEvent soc
        handle r
        handleSoc soc


data Reply  = Message MsgReply | Event Evt.Event deriving (Show, Eq)

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

receive :: Socket -> IO (Either String Reply)
receive soc = do
    reply <- getReply soc
    case reply of
        Right (ty, body) -> pure $ if testBit ty 31
            then Event `second` Evt.toEvent (ty `clearBit` 31) body
            else Message `second` toMsgReply ty body
        _ -> pure $ Left "Get Reply failed"

receiveMsg :: Socket -> IO (Either String MsgReply)
receiveMsg soc = do
    r <- getReply soc
    pure $ do
        (ty, body) <- r
        toMsgReply ty body

receiveEvent :: Socket -> IO (Either String Evt.Event)
receiveEvent soc = do
    r <- getReply soc
    pure $ do
        (ty, body) <- r
        Evt.toEvent (ty `clearBit` 31) body

connecti3 :: IO Socket
connecti3 = do
    soc  <- socket AF_UNIX Stream 0
    addr <- getSocketPath
    case addr of
        Nothing    -> putStrLn "Failed to get i3 socket path" >> exitFailure
        Just addr' -> do
            connect soc (SockAddrUnix $ BL.unpack addr')
            pure soc

runCommand :: Socket -> BL.ByteString -> IO (Either String Reply)
runCommand soc b = Msg.sendMsg soc Msg.RunCommand b >> receive soc


getWorkspaces :: Socket -> IO (Either String Reply)
getWorkspaces soc = Msg.sendMsg' soc Msg.Workspaces >> receive soc
