module I3IPC
    ( getSocketPath
    )
where

import           System.Environment                  ( lookupEnv )
import           Data.Maybe                          ( isJust )
import           System.Process.Typed                ( proc
                                                     , readProcess
                                                     )
import           System.Exit                         ( ExitCode(..) )
import qualified Data.ByteString.Lazy.Char8         as BSL


getSocketPath :: IO (Maybe BSL.ByteString)
getSocketPath = do
    res <- lookupEnv "I3SOCK"
    if isJust res
        then pure $ fmap BSL.pack res
        else do
            (exitCode, out, err) <- readProcess $ proc "i3" ["--get-socketpath"]
            if exitCode /= ExitSuccess then pure Nothing else pure $ Just out

