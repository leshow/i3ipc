module I3IPC.Message
    ( MessageType(..)
    , createMsg
    , createMsg'
    , sendMsg
    , sendMsg'
    )
where

import           Network.Socket.ByteString.Lazy
import           Network.Socket                      ( Socket )
import qualified Data.ByteString.Lazy               as BSL
import           Data.Binary.Put
import Data.Function ((&))
import           Data.Int

-- | I3 IPC Commands https://i3wm.org/docs/ipc.html#_sending_messages_to_i3
-- 
data MessageType =
    -- | Run the payload as an i3 command (like the commands you can bind to keys).
    RunCommand
    -- | Get the list of current workspaces. 
    | Workspaces
    -- | Subscribe this IPC connection to the event types specified in the message payload. 
    | Subscribe
    -- | Get the list of current outputs.
    | Outputs
    -- | Get the i3 layout tree.
    | Tree
    -- | Gets the names of all currently set marks.
    | Marks
    -- | Gets the specified bar configuration or the names of all bar configurations if payload is empty.
    | BarConfig
    -- | Gets the i3 version.
    | Version
    -- |  Gets the names of all currently configured binding modes.
    | BindingModes
    -- | Returns the last loaded i3 config. 
    | Config
    -- |  Sends a tick event with the specified payload.
    | Tick
    -- | Sends an i3 sync event with the specified random value to the specified window.
    | Sync
    deriving (Enum, Show, Eq)

-- | Create a message for i3 based on on 'MessageType' 
-- Output of the form: "i3-ipc" <msglen> <msgtype> <payload>
createMsg :: MessageType -> BSL.ByteString -> BSL.ByteString
createMsg msgtype msg = runPut $ do
    putByteString "i3-ipc"
    putWord32host $ fromIntegral (BSL.length msg)
    putWord32host $ fromIntegral (fromEnum msgtype)
    putLazyByteString msg

-- | Create a msg for i3 based on 'MessageType' without a message body, based on 'createMsg'
createMsg' :: MessageType -> BSL.ByteString
createMsg' msgtype = runPut $ do
    putByteString "i3-ipc"
    putWord32host $ fromIntegral @Int 0
    putWord32host $ fromIntegral (fromEnum msgtype)
    
-- | Send a message over the socket of 'MessageType' and some content
sendMsg :: Socket -> MessageType -> BSL.ByteString -> IO Int64
sendMsg soc msgtype msg = createMsg msgtype msg & send soc

-- | Similar to 'sendMsg' but with no message body
sendMsg' :: Socket -> MessageType -> IO Int64
sendMsg' soc msgtype = createMsg' msgtype & send soc