{-|
Related to `I3IPC.Subscribe.Subscribe', specifically, each 'I3IPC.Event.Event' constructor matches a constructor for 'I3IPC.Subscribe.Subscribe'
-}
{-# LANGUAGE RecordWildCards #-}
module I3IPC.Event where

import           I3IPC.Reply                         ( BarConfigReply
                                                     , Node
                                                     )

import           Control.Monad                       ( mzero )
import           Data.Aeson
import           Data.Aeson.Encoding                 ( text )
import qualified Data.ByteString.Lazy               as BL
import           Data.Int
import           Data.Text                           ( Text )
import           Data.Vector                         ( Vector )
import           GHC.Generics

-- | Responses to the various events you can subscribe to.
data Event =
    -- | See 'I3IPC.Event.WorkspaceEvent' for response.  Sent when the user switches to a different workspace, when a new workspace is initialized or when a workspace is removed (because the last client vanished). 
    Workspace !WorkspaceEvent
    -- | See 'I3IPC.Event.OutputEvent' for response.  Sent when RandR issues a change notification (of either screens, outputs, CRTCs or output properties). 
    | Output !OutputEvent
    -- | See 'I3IPC.Event.ModeEvent' for response.  Sent whenever i3 changes its binding mode. 
    | Mode !ModeEvent
    -- | See 'I3IPC.Event.WindowEvent' for response.  Sent when a client’s window is successfully reparented (that is when i3 has finished fitting it into a container), when a window received input focus or when certain properties of the window have changed. 
    | Window !WindowEvent
    -- | See 'I3IPC.Event.BarConfigUpdateEvent' for response.  Sent when the hidden_state or mode field in the barconfig of any bar instance was updated and when the config is reloaded.
    | BarConfigUpdate !BarConfigUpdateEvent
    -- | See 'I3IPC.Event.BindingEvent' for response. Sent when a configured command binding is triggered with the keyboard or mouse 
    | Binding !BindingEvent
    -- |  See 'I3IPC.Event.ShutdownEvent' for response. Sent when the ipc shuts down because of a restart or exit by user command 
    | Shutdown !ShutdownEvent
    -- | See 'I3IPC.Event.TickEvent' for response. Sent when the ipc client subscribes to the tick event (with "first": true) or when any ipc client sends a SEND_TICK message (with "first": false).
    | Tick !TickEvent
    deriving (Eq, Show)

toEvent' :: Int -> BL.ByteString -> Either String Event
toEvent' 0 = (Workspace <$>) . eitherDecode'
toEvent' 1 = (Output <$>) . eitherDecode'
toEvent' 2 = (Mode <$>) . eitherDecode'
toEvent' 3 = (Window <$>) . eitherDecode'
toEvent' 4 = (BarConfigUpdate <$>) . eitherDecode'
toEvent' 5 = (Binding <$>) . eitherDecode'
toEvent' 6 = (Shutdown <$>) . eitherDecode'
toEvent' 7 = (Tick <$>) . eitherDecode'
toEvent' _ = error "Unknown Event type found"

toEvent :: Int -> BL.ByteString -> Either String Event
toEvent 0 = (Workspace <$>) . eitherDecode
toEvent 1 = (Output <$>) . eitherDecode
toEvent 2 = (Mode <$>) . eitherDecode
toEvent 3 = (Window <$>) . eitherDecode
toEvent 4 = (BarConfigUpdate <$>) . eitherDecode
toEvent 5 = (Binding <$>) . eitherDecode
toEvent 6 = (Shutdown <$>) . eitherDecode
toEvent 7 = (Tick <$>) . eitherDecode
toEvent _ = error "Unknown Event type found"

data WorkspaceChange =
    Focus
    | Init
    | Empty
    | Urgent
    | Rename
    | Reload
    | Restored
    | Move
    | UnknownChange
    deriving (Eq, Generic, Show)

instance ToJSON WorkspaceChange where
    toEncoding = \case
        Focus         -> text "focus"
        Init          -> text "init"
        Empty         -> text "empty"
        Urgent        -> text "urgent"
        Rename        -> text "rename"
        Reload        -> text "reload"
        Restored      -> text "restored"
        Move          -> text "move"
        UnknownChange -> text "unknown"

instance FromJSON WorkspaceChange where
    parseJSON (String s) = pure $! case s of
        "focus"    -> Focus
        "init"     -> Init
        "empty"    -> Empty
        "urgent"   -> Rename
        "rename"   -> Rename
        "reload"   -> Reload
        "restored" -> Restored
        "move"     -> Move
        _          -> UnknownChange
    parseJSON _ = mzero

-- | Workspace Event
-- This event consists of a single serialized map containing a property change (string) which indicates the type of the change ("focus", "init", "empty", "urgent", "reload", "rename", "restored", "move"). A current (object) property will be present with the affected workspace whenever the type of event affects a workspace (otherwise, it will be +null).
-- When the change is "focus", an old (object) property will be present with the previous workspace. When the first switch occurs (when i3 focuses the workspace visible at the beginning) there is no previous workspace, and the old property will be set to null. Also note that if the previous is empty it will get destroyed when switching, but will still be present in the "old" property.
data WorkspaceEvent = WorkspaceEvent
    { wrk_change  :: !WorkspaceChange -- ^ Type of workspace change
    , wrk_current :: !(Maybe Node) -- ^ If the type of event affects the workspace this will have a Just instance
    , wrk_old     :: !(Maybe Node) -- ^ Will be Just only when change is Focus and there was a previous workspace
    }
    deriving (Eq, Generic, Show)

instance ToJSON WorkspaceEvent where
    toEncoding = genericToEncoding defaultOptions { fieldLabelModifier = drop 4
                                                  , omitNothingFields  = True
                                                  }

instance FromJSON WorkspaceEvent where
    parseJSON = withObject "WorkspaceEvent" $ \o -> do
        wrk_change  <- o .: "change"
        wrk_current <- o .:? "current"
        wrk_old     <- o .:? "old"
        pure $! WorkspaceEvent { .. }

-- | Output Event
-- This event consists of a single serialized map containing a property change (string) which indicates the type of the change (currently only "unspecified").
data OutputEvent = OutputEvent
    { output_change :: !Text -- ^ Currently only "unspecified"
    }
    deriving (Eq, Generic, Show)

instance ToJSON OutputEvent where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 7 }

instance FromJSON OutputEvent where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 7 }

-- |  Mode Event
-- This event consists of a single serialized map containing a property change (string) which holds the name of current mode in use. The name is the same as specified in config when creating a mode. The default mode is simply named default. It contains a second property, pango_markup, which defines whether pango markup shall be used for displaying this mode.
data ModeEvent = ModeEvent
    { mode_change       :: !Text -- ^ always "default"
    , mode_pango_markup :: !Bool -- ^ Whether pango markup should be used for displaying this mode
    }
    deriving (Eq, Generic, Show)


instance ToJSON ModeEvent where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 5 }

instance FromJSON ModeEvent where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }


-- | Window Event
-- This event consists of a single serialized map containing a property change (string) which indicates the type of the change
data WindowEvent = WindowEvent
    { win_change    :: !WindowChange
    , win_container :: !Node -- ^ Additionally a container (object) field will be present, which consists of the window’s parent container. Be aware that for the "new" event, the container will hold the initial name of the newly reparented window (e.g. if you run urxvt with a shell that changes the title, you will still at this point get the window title as "urxvt").
    }
    deriving (Eq, Show, Generic)

instance ToJSON WindowEvent where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON WindowEvent where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }

data WindowChange =
    WinNew -- ^  the window has become managed by i3 
    | WinClose -- ^ the window has closed 
    | WinFocus -- ^ the window has received input focus 
    | WinTitle -- ^ the window’s title has changed 
    | WinFullscreenMode -- ^ the window has entered or exited fullscreen mode 
    | WinMove -- ^ the window has changed its position in the tree 
    | WinFloating -- ^ the window has transitioned to or from floating 
    | WinUrgent -- ^ the window has become urgent or lost its urgent status 
    | WinMark -- ^ a mark has been added to or removed from the window 
    | WinUnknown -- ^ an unknown change, submit a PR to add a new WindowChange if you get this
    deriving (Eq, Show, Generic)

instance ToJSON WindowChange where
    toEncoding = \case
        WinNew            -> text "new"
        WinFocus          -> text "focus"
        WinTitle          -> text "title"
        WinFullscreenMode -> text "fullscreen_mode"
        WinMove           -> text "move"
        WinFloating       -> text "floating"
        WinUrgent         -> text "urgent"
        WinMark           -> text "mark"
        WinClose          -> text "close"
        WinUnknown        -> text "unknown"

instance FromJSON WindowChange where
    parseJSON (String s) = pure $! case s of
        "new"             -> WinNew
        "focus"           -> WinFocus
        "title"           -> WinTitle
        "fullscreen_mode" -> WinFullscreenMode
        "move"            -> WinMove
        "floating"        -> WinFloating
        "urgent"          -> WinUrgent
        "mark"            -> WinMark
        "close"           -> WinClose
        _                 -> WinUnknown
    parseJSON _ = mzero


-- | BarConfig_Update Event
-- This event consists of a single serialized map reporting on options from the barconfig of the specified bar_id that were updated in i3. This event is the same as a GET_BAR_CONFIG reply for the bar with the given id.
type BarConfigUpdateEvent = BarConfigReply

-- | Binding Event
-- This event consists of a single serialized map reporting on the details of a binding that ran a command because of user input. The change (string) field indicates what sort of binding event was triggered (right now it will always be "run" but may be expanded in the future).
data BindingEvent = BindingEvent
    { bind_change  :: !Text -- ^ right now this is always "run"
    , bind_binding :: !BindingObject -- ^ Details about the binding that was run
    }
    deriving (Eq, Show, Generic)

instance ToJSON BindingEvent where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 5 }

instance FromJSON BindingEvent where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

data BindingObject = BindingObject
    { bind_command          :: !Text
    , bind_event_state_mask :: !(Vector Text)
    , bind_input_code       :: !Int32
    , bind_symbol           :: !(Maybe Text)
    , bind_input_type       :: !BindType
    }
    deriving (Eq, Show, Generic)

instance ToJSON BindingObject where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 5 }

instance FromJSON BindingObject where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

-- | Bind type
data BindType = Keyboard | Mouse  deriving (Eq, Show, Generic)

instance FromJSON BindType where
    parseJSON (String s) = pure $! case s of
        "keyboard" -> Keyboard
        "mouse"    -> Mouse
        _          -> error "Found BindType not recognized"
    parseJSON _ = mzero

instance ToJSON BindType where
    toEncoding = \case
        Keyboard -> text "keyboard"
        Mouse    -> text "mouse"

-- | Shutdown Event
-- This event is triggered when the connection to the ipc is about to shutdown because of a user action such as a restart or exit command. The change (string) field indicates why the ipc is shutting down. It can be either "restart" or "exit".
data ShutdownEvent = ShutdownEvent
    { shutdown_change :: !ShutdownChange
    }
    deriving (Eq, Show, Generic)

instance FromJSON ShutdownEvent where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 9 }

instance ToJSON ShutdownEvent where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 9 }

data ShutdownChange = Restart | Exit deriving (Eq, Show, Generic)

instance FromJSON ShutdownChange where
    parseJSON (String s) = pure $! case s of
        "restart" -> Restart
        "exit"    -> Exit
        _         -> error "Found ShutdownChange not recognized"
    parseJSON _ = mzero

instance ToJSON ShutdownChange where
    toEncoding = \case
        Restart -> text "restart"
        Exit    -> text "exit"

-- | Tick Event
-- This event is triggered by a subscription to tick events or by a 'I3IPC.Message.Tick' message.
data TickEvent = TickEvent
    { tick_first   :: !Bool
    , tick_payload :: !Text
    }
    deriving (Eq, Show, Generic)

instance ToJSON TickEvent where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 5 }

instance FromJSON TickEvent where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }
