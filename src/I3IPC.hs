module I3IPC
    ( getSocketPath
    , run
    )
where

import           I3IPC.Message
import           I3IPC.Subscribe
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
import           Network.Socket
import           Control.Monad.Trans                 ( liftIO )


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
subscribe :: [Subscribe] -> IO ()
subscribe subtypes = do
    soc  <- liftIO $ socket AF_UNIX Stream 0
    addr <- liftIO getSocketPath
    case addr of
        Nothing -> do
            putStrLn "Failed to get i3 socket path"
            exitFailure
        Just addr' -> do
            liftIO $ connect soc (SockAddrUnix $ BSL.unpack addr')
            liftIO $ sendMsg soc Subscribe subtypes
            pure ()


