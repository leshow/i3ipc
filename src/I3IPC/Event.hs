module I3IPC.Event
    ( EventType(..)
    , WorkspaceChange(..)
    , WorkspaceEvent
    )
where

import           I3IPC.Reply

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Encoding                 ( text )
import           Data.Vector                         ( Vector )

-- | I3 'EventType' https://i3wm.org/docs/ipc.html#_events
data EventType =
    -- | Sent when the user switches to a different workspace, when a new workspace is initialized or when a workspace is removed (because the last client vanished). 
    Workspace
    -- | Sent when RandR issues a change notification (of either screens, outputs, CRTCs or output properties). 
    | Output
    -- | Sent whenever i3 changes its binding mode. 
    | Mode
    -- | Sent when a client’s window is successfully reparented (that is when i3 has finished fitting it into a container), when a window received input focus or when certain properties of the window have changed. 
    | Window
    -- | Sent when the hidden_state or mode field in the barconfig of any bar instance was updated and when the config is reloaded.
    | BarConfigUpdate
    -- | Sent when a configured command binding is triggered with the keyboard or mouse 
    | Binding
    -- | Sent when the ipc shuts down because of a restart or exit by user command 
    | Shutdown
    -- | Sent when the ipc client subscribes to the tick event (with "first": true) or when any ipc client sends a SEND_TICK message (with "first": false).
    | Tick
    deriving (Enum, Eq, Show)

data WorkspaceChange =
    Focus
    | Init
    | Empty
    | Urgent
    | Rename
    | Reload
    | Restored
    | Move
    deriving (Eq, Generic, Show)

instance ToJSON WorkspaceChange where
    toEncoding = \case
        Focus    -> text "focus"
        Init     -> text "init"
        Empty    -> text "empty"
        Urgent   -> text "urgent"
        Rename   -> text "rename"
        Reload   -> text "reload"
        Restored -> text "restored"
        Move     -> text "move"

instance FromJSON WorkspaceChange where
    parseJSON (String s) = pure $ case s of
        "focus"    -> Focus
        "init"     -> Init
        "empty"    -> Empty
        "urgent"   -> Rename
        "rename"   -> Rename
        "reload"   -> Reload
        "restored" -> Restored
        "move"     -> Move
        _          -> error "Received unrecognized WorkspaceChange"
    parseJSON _ = error "Error parsing WorkspaceChange"

data WorkspaceEvent = WorkspaceEvent {
    change :: !WorkspaceChange
    , current :: !(Maybe Node)
    , old :: !(Maybe Node)
} deriving (Eq, Generic, Show)

-- data OutputEvent = OutputEvent {
--     outChange :: !OutputChange
-- } deriving (Eq, Generic, Show)
