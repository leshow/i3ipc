module I3IPC
    ( module Msg
    , module Sub
    , module Evt
    , getSocketPath
    , subscribe
    , receive
    )
where

import qualified I3IPC.Message                      as Msg
import qualified I3IPC.Subscribe                    as Sub
import qualified I3IPC.Event                        as Evt

import           System.Environment                  ( lookupEnv )
import           Data.Maybe                          ( isJust
                                                     , isNothing
                                                     , fromJust
                                                     )
import           System.Process.Typed                ( proc
                                                     , readProcess
                                                     )
import           System.Exit                         ( ExitCode(..)
                                                     , exitFailure
                                                     )
import qualified Data.ByteString.Lazy.Char8         as BSL
import qualified Data.ByteString                    as BS
import           Network.Socket               hiding ( send
                                                     , sendTo
                                                     , recv
                                                     , recvFrom
                                                     )
import           Network.Socket.ByteString.Lazy
import           Data.Aeson                          ( encode
                                                     , decode
                                                     , Value
                                                     )
import           Data.Binary.Get
import           Data.Bits                           ( testBit
                                                     , clearBit
                                                     )
import           Data.Word

getSocketPath :: IO (Maybe BSL.ByteString)
getSocketPath = do
    res <- lookupEnv "I3SOCK"
    if isJust res
        then pure $ fmap BSL.pack res
        else do
            (exitCode, out, err) <- readProcess $ proc "i3" ["--get-socketpath"]
            if exitCode /= ExitSuccess
                then pure Nothing
                else pure $ Just (BSL.filter (/= '\n') out)


-- | Subscribe to i3 msgs of the specific 'ReplyType'
--
subscribe :: [Sub.Subscribe] -> IO ()
subscribe subtypes = do
    soc  <- socket AF_UNIX Stream 0
    addr <- getSocketPath
    case addr of
        Nothing    -> putStrLn "Failed to get i3 socket path" >> exitFailure
        Just addr' -> do
            connect soc (SockAddrUnix $ BSL.unpack addr')
            Msg.sendMsg soc Msg.Subscribe (encode subtypes)
            handleSoc soc
            close soc
    where handleSoc soc = undefined

data Reply = Reply {
    len :: !Word32
    , msgtype :: !Word32
    , body :: BS.ByteString
} deriving (Show)

data ReplyType = Message Msg.MessageType | Event Evt.EventType deriving (Show, Eq)

receive :: Socket -> IO (Maybe (ReplyType, Value))
receive soc = do
    magic <- recv soc 6
    if magic == "i3-ipc"
        then do
            msglen  <- fromIntegral . runGet getWord32le <$> recv soc 4
            rType   <- fromIntegral . runGet getWord32le <$> recv soc 4
            msgbody <- recv soc msglen
            let replyType = if testBit rType 31
                    then Event (toEnum (rType `clearBit` 31))
                    else Message (toEnum rType)
            pure $ do
                body <- decode msgbody
                pure (replyType, body)
        else pure Nothing

getReply :: Get Reply
getReply = do
    len     <- getWord32le
    msgtype <- getWord32le
    body    <- getByteString (fromIntegral len)
    return $! Reply len msgtype body
