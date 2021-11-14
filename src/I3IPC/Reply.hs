module I3IPC.Reply where

import           Control.Monad                       ( mzero )
import           Data.Aeson
import           Data.Aeson.Encoding                 ( text )
import qualified Data.ByteString.Lazy               as BL
import           Data.Int
import           Data.Map.Strict                     ( Map )
import           Data.Text                           ( Text )
import           Data.Vector                         ( Vector )
import           GHC.Generics

data MsgReply =
    -- | Run the payload as an i3 command (like the commands you can bind to keys).
    RunCommand !Success
    -- | Get the list of current workspaces. 
    | Workspaces !WorkspaceReply
    -- | Subscribe this IPC connection to the event types specified in the message payload. 
    | Subscribe !Success
    -- | Get the list of current outputs.
    | Outputs !OutputsReply
    -- | Get the i3 layout tree.
    | Tree !Node
    -- | Gets the names of all currently set marks.
    | Marks !MarksReply
    -- | Gets the specified bar configuration or the names of all bar configurations if payload is empty.
    | BarConfig !BarConfigReply
    -- | Gets the i3 version.
    | Version !VersionReply
    -- |  Gets the names of all currently configured binding modes.
    | BindingModes !BindingModesReply
    -- | Returns the last loaded i3 config. 
    | Config !ConfigReply
    -- |  Sends a tick event with the specified payload.
    | Tick !Success
    -- | Sends an i3 sync event with the specified random value to the specified window.
    | Sync !Success
    deriving (Show, Eq)

toMsgReply' :: Int -> BL.ByteString -> Either String MsgReply
toMsgReply' 0  = (RunCommand <$>) . eitherDecode'
toMsgReply' 1  = (Workspaces <$>) . eitherDecode'
toMsgReply' 2  = (Subscribe <$>) . eitherDecode'
toMsgReply' 3  = (Outputs <$>) . eitherDecode'
toMsgReply' 4  = (Tree <$>) . eitherDecode'
toMsgReply' 5  = (Marks <$>) . eitherDecode'
toMsgReply' 6  = (BarConfig <$>) . eitherDecode'
toMsgReply' 7  = (Version <$>) . eitherDecode'
toMsgReply' 8  = (BindingModes <$>) . eitherDecode'
toMsgReply' 9  = (Config <$>) . eitherDecode'
toMsgReply' 10 = (Tick <$>) . eitherDecode'
toMsgReply' 11 = (Sync <$>) . eitherDecode'
toMsgReply' _  = error "Unknown Event type found"

toMsgReply :: Int -> BL.ByteString -> Either String MsgReply
toMsgReply 0  = (RunCommand <$>) . eitherDecode
toMsgReply 1  = (Workspaces <$>) . eitherDecode
toMsgReply 2  = (Subscribe <$>) . eitherDecode
toMsgReply 3  = (Outputs <$>) . eitherDecode
toMsgReply 4  = (Tree <$>) . eitherDecode
toMsgReply 5  = (Marks <$>) . eitherDecode
toMsgReply 6  = (BarConfig <$>) . eitherDecode
toMsgReply 7  = (Version <$>) . eitherDecode
toMsgReply 8  = (BindingModes <$>) . eitherDecode
toMsgReply 9  = (Config <$>) . eitherDecode
toMsgReply 10 = (Tick <$>) . eitherDecode
toMsgReply 11 = (Sync <$>) . eitherDecode
toMsgReply _  = error "Unknown Event type found"

decodeBarIds :: BL.ByteString -> Either String BarIds
decodeBarIds = eitherDecode

-- | Success Reply
-- used for Sync, Subscribe, Command, Tick
data Success = Success
    { success :: !Bool
    }
    deriving (Eq, Show, Generic, FromJSON)

instance ToJSON Success where
    toEncoding = genericToEncoding defaultOptions

-- | Workspaces Reply
-- The reply consists of a serialized list of workspaces. 
data WorkspaceReply = WorkspaceReply !(Vector Workspace)
    deriving (Eq, Generic, Show)

instance ToJSON WorkspaceReply where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON WorkspaceReply where
    parseJSON = genericParseJSON defaultOptions

data Workspace = Workspace
    { ws_num     :: !Int32 -- ^ The logical number of the workspace. Corresponds to the command to switch to this workspace. For named workspaces, this will be -1. 
    , ws_name    :: !Text -- ^ The name of this workspace (by default num+1), as changed by the user. Encoded in UTF-8. 
    , ws_visible :: !Bool -- ^ Whether this workspace is currently visible on an output (multiple workspaces can be visible at the same time). 
    , ws_focused :: !Bool -- ^ Whether this workspace currently has the focus (only one workspace can have the focus at the same time).
    , ws_urgent  :: !Bool -- ^ Whether a window on this workspace has the "urgent" flag set. 
    , ws_rect    :: !Rect -- ^ The rectangle of this workspace (equals the rect of the output it is on), consists of x, y, width, height. 
    , ws_output  :: !Text -- ^ The video output this workspace is on (LVDS1, VGA1, …). 
    }
    deriving (Eq, Generic, Show)

instance ToJSON Workspace where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 3 }

instance FromJSON Workspace where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 3 }

-- | Outputs Reply
-- The reply consists of a serialized list of outputs. 
data OutputsReply = OutputsReply !(Vector Output)
    deriving (Eq, Generic, Show)

instance ToJSON OutputsReply where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON OutputsReply where
    parseJSON = genericParseJSON defaultOptions

data Output = Output
    { output_name              :: !Text
    , output_active            :: !Bool
    , output_primary           :: !Bool
    , output_current_workspace :: !(Maybe Text)
    , output_rect              :: !Rect
    }
    deriving (Eq, Show, Generic)

instance ToJSON Output where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 7 }

instance FromJSON Output where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 7 }

-- | Tree Reply
-- The reply consists of a serialized tree. Each node in the tree (representing one container) has at least the properties listed below. While the nodes might have more properties, please do not use any properties which are not documented here. They are not yet finalized and will probably change!
data Node = Node
    { node_id                   :: !Int -- ^ The internal ID (actually a C pointer value) of this container. Do not make any assumptions about it. You can use it to (re-)identify and address containers when talking to i3. 
    , node_name                 :: !(Maybe Text) -- ^ The internal name of this container. For all containers which are part of the tree structure down to the workspace contents, this is set to a nice human-readable name of the container. For containers that have an X11 window, the content is the title (_NET_WM_NAME property) of that window. For all other containers, the content is not defined (yet). 
    , node_type                 :: !NodeType -- ^ Type of this container. Can be one of "root", "output", "con", "floating_con", "workspace" or "dockarea". 
    , node_output               :: !(Maybe Text) -- ^ The X11 display the node is drawn in. ex. "DP-4.8"
    , node_orientation          :: !NodeOrientation -- ^ Can either be "horizontal" "vertical" or "none"
    , node_border               :: !NodeBorder -- ^ Can be either "normal", "none" or "pixel", depending on the container’s border style. 
    , node_current_border_width :: !Int32 -- ^ Number of pixels of the border width. 
    , node_layout               :: !NodeLayout -- ^ Can be either "splith", "splitv", "stacked", "tabbed", "dockarea" or "output". Other values might be possible in the future, should we add new layouts. 
    , node_percent              :: !(Maybe Float) -- ^ The percentage which this container takes in its parent. A value of null means that the percent property does not make sense for this container, for example for the root container. 
    , node_rect                 :: !Rect -- ^ The absolute display coordinates for this container. Display coordinates means that when you have two 1600x1200 monitors on a single X11 Display (the standard way), the coordinates of the first window on the second monitor are { "x": 1600, "y": 0, "width": 1600, "height": 1200 }. 
    , node_window_rect          :: !Rect -- ^ The coordinates of the actual client window inside its container. These coordinates are relative to the container and do not include the window decoration (which is actually rendered on the parent container). So, when using the default layout, you will have a 2 pixel border on each side, making the window_rect { "x": 2, "y": 0, "width": 632, "height": 366 } (for example). 
    , node_deco_rect            :: !Rect -- ^ The coordinates of the window decoration inside its container. These coordinates are relative to the container and do not include the actual client window. 
    , node_geometry             :: !Rect -- ^ The original geometry the window specified when i3 mapped it. Used when switching a window to floating mode, for example. 
    , node_window               :: !(Maybe Int32) -- ^ The X11 window ID of the actual client window inside this container. This field is set to null for split containers or otherwise empty containers. This ID corresponds to what xwininfo(1) and other X11-related tools display (usually in hex). 
    , node_window_properties    :: !(Maybe (Map WindowProperty (Maybe Text))) -- ^ X11 window properties title, instance, class, window_role and transient_for. 
    , node_urgent               :: !Bool -- ^ Whether this container (window, split container, floating container or workspace) has the urgency hint set, directly or indirectly. All parent containers up until the workspace container will be marked urgent if they have at least one urgent child. 
    , node_focused              :: !Bool -- ^ Whether this container is currently focused. 
    , node_focus                :: !(Vector Int64) -- ^ List of child node IDs (see nodes, floating_nodes and id) in focus order. Traversing the tree by following the first entry in this array will result in eventually reaching the one node with focused set to true. 
    , node_sticky               :: !Bool
    , node_floating_nodes       :: !(Vector Node) -- ^ The floating child containers of this node. Only non-empty on nodes with type workspace. 
    , node_nodes                :: !(Vector Node) -- ^ The tiling (i.e. non-floating) child containers of this node. 
    }
    deriving (Eq, Generic, Show)

data NodeOrientation =
    Horizontal
    | Vertical
    | OrientNone
    | UnknownOrient
    deriving (Eq, Generic, Show)

instance FromJSON NodeOrientation where
    parseJSON (String s) = pure $! case s of
        "none"       -> OrientNone
        "horizontal" -> Horizontal
        "vertical"   -> Vertical
        _            -> UnknownOrient
    parseJSON _ = mzero

instance ToJSON NodeOrientation where
    toEncoding = \case
        OrientNone    -> text "none"
        Vertical      -> text "vertical"
        Horizontal    -> text "horizontal"
        UnknownOrient -> text "unknown"

instance ToJSON Node where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 5 }

instance FromJSON Node where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 5 }

data WindowProperty =
    Title
    | Instance
    | Class
    | WindowRole
    | TransientFor
    | Machine
    | UnknownProperty
    deriving (Eq, Enum, Ord, Generic, Show)

instance FromJSONKey WindowProperty where
    fromJSONKey = FromJSONKeyText f
      where
        f x = case x of
            "title"         -> Title
            "instance"      -> Instance
            "class"         -> Class
            "window_role"   -> WindowRole
            "transient_for" -> TransientFor
            "machine"       -> Machine
            _               -> UnknownProperty

instance ToJSONKey WindowProperty where
    toJSONKey = ToJSONKeyText f g
      where
        f x = case x of
            Title           -> "title"
            Instance        -> "instance"
            Class           -> "class"
            WindowRole      -> "window_role"
            TransientFor    -> "transient_for"
            Machine         -> "machine"
            UnknownProperty -> "unknown"
        g x = case x of
            Title           -> text "title"
            Instance        -> text "instance"
            Class           -> text "class"
            WindowRole      -> text "window_role"
            TransientFor    -> text "transient_for"
            Machine         -> text "machine"
            UnknownProperty -> text "unknown"

instance FromJSON WindowProperty where
    parseJSON (String s) = pure $! case s of
        "title"         -> Title
        "instance"      -> Instance
        "class"         -> Class
        "window_role"   -> WindowRole
        "transient_for" -> TransientFor
        "machine"       -> Machine
        _               -> UnknownProperty
    parseJSON _ = mzero


instance ToJSON WindowProperty where
    toEncoding = \case
        Title           -> text "title"
        Instance        -> text "instance"
        Class           -> text "class"
        WindowRole      -> text "window_role"
        TransientFor    -> text "transient_for"
        Machine         -> text "machine"
        UnknownProperty -> text "unknown"

-- | Marks Reply
-- The reply consists of a single array of strings for each container that has a mark. A mark can only be set on one container, so the array is unique. The order of that array is undefined.
-- If no window has a mark the response will be the empty array [].
data MarksReply = MarksReply !(Vector Text)
    deriving (Eq, Generic, Show, FromJSON)

instance ToJSON MarksReply where
    toEncoding = genericToEncoding defaultOptions

data NodeBorder =
    Normal
    | None
    | Pixel
    | UnknownBorder
    deriving (Eq, Generic, Show)

instance ToJSON NodeBorder where
    toEncoding = \case
        Normal        -> text "normal"
        None          -> text "none"
        Pixel         -> text "pixel"
        UnknownBorder -> text "unknown"

instance FromJSON NodeBorder where
    parseJSON (String s) = pure $! case s of
        "normal" -> Normal
        "none"   -> None
        "pixel"  -> Pixel
        _        -> UnknownBorder
    parseJSON _ = mzero

data Rect = Rect
    { x      :: !Int32
    , y      :: !Int32
    , width  :: !Int32
    , height :: !Int32
    }
    deriving (Eq, Generic, Show, FromJSON)

instance ToJSON Rect where
    toEncoding = genericToEncoding defaultOptions

data NodeType =
    RootType
    | OutputType
    | ConType
    | FloatingConType
    | WorkspaceType
    | DockAreaType
    | UnknownType
    deriving (Eq, Generic, Show)

instance ToJSON NodeType where
    toEncoding = \case
        RootType        -> text "root"
        OutputType      -> text "output"
        ConType         -> text "con"
        FloatingConType -> text "floating_con"
        WorkspaceType   -> text "workspace"
        DockAreaType    -> text "dockarea"
        UnknownType     -> text "unknown"

instance FromJSON NodeType where
    parseJSON (String s) = pure $! case s of
        "root"         -> RootType
        "output"       -> OutputType
        "con"          -> ConType
        "floating_con" -> FloatingConType
        "workspace"    -> WorkspaceType
        "dockarea"     -> DockAreaType
        _              -> UnknownType
    parseJSON _ = mzero

data NodeLayout =
    SplitHorizontalLayout
    | SplitVerticalLayout
    | StackedLayout
    | TabbedLayout
    | DockAreaLayout
    | OutputLayout
    | UnknownLayout
    deriving (Eq, Generic, Show)

instance ToJSON NodeLayout where
    toEncoding = \case
        SplitHorizontalLayout -> text "splith"
        SplitVerticalLayout   -> text "splitv"
        StackedLayout         -> text "stacked"
        TabbedLayout          -> text "tabbed"
        DockAreaLayout        -> text "dockarea"
        OutputLayout          -> text "output"
        UnknownLayout         -> text "unknown"

instance FromJSON NodeLayout where
    parseJSON (String s) = pure $! case s of
        "splith"   -> SplitHorizontalLayout
        "splitv"   -> SplitVerticalLayout
        "stacked"  -> StackedLayout
        "tabbed"   -> TabbedLayout
        "dockarea" -> DockAreaLayout
        "output"   -> OutputLayout
        _          -> UnknownLayout
    parseJSON _ = mzero

-- | BarConfig Reply
-- This can be used by third-party workspace bars (especially i3bar, but others are free to implement compatible alternatives) to get the bar block configuration from i3.
data BarIds = BarIds !(Vector Text)
    deriving (Eq, Generic, Show)

instance ToJSON BarIds where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON BarIds where
    parseJSON = genericParseJSON defaultOptions

data BarConfigReply = BarConfigReply
    { bar_id                     :: !Text
    , bar_mode                   :: !Text
    , bar_position               :: !Text
    , bar_status_command         :: !Text
    , bar_font                   :: !Text
    , bar_workspace_buttons      :: !Bool
    , bar_binding_mode_indicator :: !Bool
    , bar_verbose                :: !Bool
    , bar_colors                 :: !(Map BarPart Text)
    }
    deriving (Eq, Generic, Show)

instance ToJSON BarConfigReply where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 4 }

instance FromJSON BarConfigReply where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }

data BarPart =
    Background
    | Statusline
    | Separator
    | FocusedBackground
    | FocusedStatusline
    | FocusedSeparator
    | FocusedWorkspaceText
    | FocusedWorkspaceBg
    | FocusedWorkspaceBorder
    | ActiveWorkspaceText
    | ActiveWorkspaceBg
    | ActiveWorkspaceBorder
    | InactiveWorkspaceText
    | InactiveWorkspaceBg
    | InactiveWorkspaceBorder
    | UrgentWorkspaceText
    | UrgentWorkspaceBg
    | UrgentWorkspaceBorder
    | BindingModeText
    | BindingModeBg
    | BindingModeBorder
    | UnknownBarPart
    deriving (Eq, Enum, Ord, Generic, Show, FromJSONKey)

instance ToJSONKey BarPart where
    toJSONKey = ToJSONKeyText f g
      where
        f x = case x of
            Background              -> "background"
            Statusline              -> "statusline"
            Separator               -> "separator"
            FocusedBackground       -> "focused_background"
            FocusedStatusline       -> "focused_statusline"
            FocusedSeparator        -> "focused_separator"
            FocusedWorkspaceText    -> "focused_workspace_text"
            FocusedWorkspaceBg      -> "focused_workspace_bg"
            FocusedWorkspaceBorder  -> "focused_workspace_border"
            ActiveWorkspaceText     -> "active_workspace_text"
            ActiveWorkspaceBg       -> "active_workspace_bg"
            ActiveWorkspaceBorder   -> "active_workspace_border"
            InactiveWorkspaceText   -> "inactive_workspace_text"
            InactiveWorkspaceBg     -> "inactive_workspace_bg"
            InactiveWorkspaceBorder -> "inactive_workspace_border"
            UrgentWorkspaceText     -> "urgent_workspace_text"
            UrgentWorkspaceBg       -> "urgent_workspace_bg"
            UrgentWorkspaceBorder   -> "urgent_workspace_border"
            BindingModeText         -> "binding_mode_text"
            BindingModeBg           -> "binding_mode_bg"
            BindingModeBorder       -> "binding_mode_border"
            UnknownBarPart          -> "unknown"
        g x = case x of
            Background              -> text "background"
            Statusline              -> text "statusline"
            Separator               -> text "separator"
            FocusedBackground       -> text "focused_background"
            FocusedStatusline       -> text "focused_statusline"
            FocusedSeparator        -> text "focused_separator"
            FocusedWorkspaceText    -> text "focused_workspace_text"
            FocusedWorkspaceBg      -> text "focused_workspace_bg"
            FocusedWorkspaceBorder  -> text "focused_workspace_border"
            ActiveWorkspaceText     -> text "active_workspace_text"
            ActiveWorkspaceBg       -> text "active_workspace_bg"
            ActiveWorkspaceBorder   -> text "active_workspace_border"
            InactiveWorkspaceText   -> text "inactive_workspace_text"
            InactiveWorkspaceBg     -> text "inactive_workspace_bg"
            InactiveWorkspaceBorder -> text "inactive_workspace_border"
            UrgentWorkspaceText     -> text "urgent_workspace_text"
            UrgentWorkspaceBg       -> text "urgent_workspace_bg"
            UrgentWorkspaceBorder   -> text "urgent_workspace_border"
            BindingModeText         -> text "binding_mode_text"
            BindingModeBg           -> text "binding_mode_bg"
            BindingModeBorder       -> text "binding_mode_border"
            UnknownBarPart          -> text "unknown"

instance FromJSON BarPart where
    parseJSON (String s) = pure $! case s of
        "background"                -> Background
        "statusline"                -> Statusline
        "separator"                 -> Separator
        "focused_background"        -> FocusedBackground
        "focused_statusline"        -> FocusedStatusline
        "focused_separator"         -> FocusedSeparator
        "focused_workspace_text"    -> FocusedWorkspaceText
        "focused_workspace_bg"      -> FocusedWorkspaceBg
        "focused_workspace_border"  -> FocusedWorkspaceBorder
        "active_workspace_text"     -> ActiveWorkspaceText
        "active_workspace_bg"       -> ActiveWorkspaceBg
        "active_workspace_border"   -> ActiveWorkspaceBorder
        "inactive_workspace_text"   -> InactiveWorkspaceText
        "inactive_workspace_bg"     -> InactiveWorkspaceBg
        "inactive_workspace_border" -> InactiveWorkspaceBorder
        "urgent_workspace_text"     -> UrgentWorkspaceText
        "urgent_workspace_bg"       -> UrgentWorkspaceBg
        "urgent_workspace_border"   -> UrgentWorkspaceBorder
        "binding_mode_text"         -> BindingModeText
        "binding_mode_bg"           -> BindingModeBg
        "binding_mode_border"       -> BindingModeBorder
        _                           -> UnknownBarPart
    parseJSON _ = mzero

instance ToJSON BarPart where
    toEncoding = \case
        Background              -> text "background"
        Statusline              -> text "statusline"
        Separator               -> text "separator"
        FocusedBackground       -> text "focused_background"
        FocusedStatusline       -> text "focused_statusline"
        FocusedSeparator        -> text "focused_separator"
        FocusedWorkspaceText    -> text "focused_workspace_text"
        FocusedWorkspaceBg      -> text "focused_workspace_bg"
        FocusedWorkspaceBorder  -> text "focused_workspace_border"
        ActiveWorkspaceText     -> text "active_workspace_text"
        ActiveWorkspaceBg       -> text "active_workspace_bg"
        ActiveWorkspaceBorder   -> text "active_workspace_border"
        InactiveWorkspaceText   -> text "inactive_workspace_text"
        InactiveWorkspaceBg     -> text "inactive_workspace_bg"
        InactiveWorkspaceBorder -> text "inactive_workspace_border"
        UrgentWorkspaceText     -> text "urgent_workspace_text"
        UrgentWorkspaceBg       -> text "urgent_workspace_bg"
        UrgentWorkspaceBorder   -> text "urgent_workspace_border"
        BindingModeText         -> text "binding_mode_text"
        BindingModeBg           -> text "binding_mode_bg"
        BindingModeBorder       -> text "binding_mode_border"
        UnknownBarPart          -> text "unknown"

-- | Version Reply
data VersionReply = VersionReply
    { v_major                   :: !Int32
    , v_minor                   :: !Int32
    , v_patch                   :: !Int32
    , v_human_readable          :: !Text
    , v_loaded_config_file_name :: !Text
    }
    deriving (Eq, Generic, Show)

instance ToJSON VersionReply where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 2 }

instance FromJSON VersionReply where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }

-- | BindingModes Reply
-- The reply consists of an array of all currently configured binding modes.
data BindingModesReply = BindingModesReply !(Vector Text)
    deriving (Eq, Generic, Show)

instance ToJSON BindingModesReply where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON BindingModesReply where
    parseJSON = genericParseJSON defaultOptions

-- | Config Reply
-- The config reply is a map which currently only contains the "config" member, which is a string containing the config file as loaded by i3 most recently.
data ConfigReply = ConfigReply
    { c_config :: !Text
    }
    deriving (Eq, Generic, Show)


instance ToJSON ConfigReply where
    toEncoding =
        genericToEncoding defaultOptions { fieldLabelModifier = drop 2 }

instance FromJSON ConfigReply where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 2 }
