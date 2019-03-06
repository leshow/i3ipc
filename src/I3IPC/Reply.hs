{-# LANGUAGE RecordWildCards #-}
module I3IPC.Reply
    ( Node
    , NodeBorder(..)
    , Rect
    , NodeType(..)
    , NodeLayout(..)
    , Command
    , Workspace
    )
where

import           GHC.Generics
import           Control.Monad                       ( mzero )
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.Encoding                 ( text )
import           Data.Int
import           Data.Vector                         ( Vector )
import           Data.Text                           ( Text )

-- | Command Reply
data Command = Command {
    cmd_success :: !Bool
} deriving (Eq, Show, Generic)

instance ToJSON Command where
    toEncoding Command { cmd_success } = pairs ("success" .= cmd_success)

instance FromJSON Command where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }

-- | Workspaces Reply
data Workspace = Workspace {
    ws_num :: !Int32 -- ^ The logical number of the workspace. Corresponds to the command to switch to this workspace. For named workspaces, this will be -1. 
    , ws_name :: !Text -- ^ The name of this workspace (by default num+1), as changed by the user. Encoded in UTF-8. 
    , ws_visible :: !Bool -- ^ Whether this workspace is currently visible on an output (multiple workspaces can be visible at the same time). 
    , ws_focused :: !Bool -- ^ Whether this workspace currently has the focus (only one workspace can have the focus at the same time).
    , ws_urgent :: !Bool -- ^ Whether a window on this workspace has the "urgent" flag set. 
    , ws_rect :: !Rect -- ^ The rectangle of this workspace (equals the rect of the output it is on), consists of x, y, width, height. 
    , ws_output :: !Text -- ^ The video output this workspace is on (LVDS1, VGA1, …). 
} deriving (Eq, Generic, Show)

instance ToJSON Workspace where
    toEncoding Workspace {..} = pairs
        ( "num" .= ws_num
        <> "name" .= ws_name
        <> "visible" .= ws_visible
        <> "focused" .= ws_focused
        <> "urgent" .= ws_urgent
        <> "rect" .= ws_rect
        <> "output" .= ws_output
        )

instance FromJSON Workspace where
    parseJSON = withObject "Workspaces" $ \o -> do
        ws_num     <- o .: "num"
        ws_name    <- o .: "name"
        ws_visible <- o .: "visible"
        ws_focused <- o .: "focused"
        ws_urgent  <- o .: "urgent"
        ws_rect    <- o .: "rect"
        ws_output  <- o .: "output"
        pure $! Workspace { .. }

-- | Subscribe Reply
data Subscribe = Subscribe {
    sub_success :: !Bool
} deriving (Eq, Show, Generic)

instance ToJSON Subscribe where
    toEncoding Subscribe { sub_success } = pairs ("success" .= sub_success)

instance FromJSON Subscribe where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 4 }

-- | Outputs Reply
data Outputs = Outputs {
    output_name :: !Text
    , output_active :: !Bool
    , output_primary :: !Bool
    , output_current_workspace :: !Text
    , output_rect :: !Rect
} deriving (Eq, Show, Generic)

instance ToJSON Outputs where
    toEncoding Outputs { .. } = pairs 
        ( "name" .= output_name 
        <> "active" .= output_active 
        <> "primary" .= output_primary
        <> "current_workspace" .= output_current_workspace
        <> "rect" .= output_rect
        )

instance FromJSON Outputs where
    parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = drop 7 }

-- | Tree Reply
data Node = Node {
    node_id :: !Int64 -- ^ The internal ID (actually a C pointer value) of this container. Do not make any assumptions about it. You can use it to (re-)identify and address containers when talking to i3. 
    , name :: !(Maybe Text) -- ^ The internal name of this container. For all containers which are part of the tree structure down to the workspace contents, this is set to a nice human-readable name of the container. For containers that have an X11 window, the content is the title (_NET_WM_NAME property) of that window. For all other containers, the content is not defined (yet). 
    , nodetype :: !NodeType -- ^ Type of this container. Can be one of "root", "output", "con", "floating_con", "workspace" or "dockarea". 
    , border :: !NodeBorder -- ^ Can be either "normal", "none" or "pixel", depending on the container’s border style. 
    , current_border_width :: !Int32 -- ^ Number of pixels of the border width. 
    , layout :: !NodeLayout -- ^ Can be either "splith", "splitv", "stacked", "tabbed", "dockarea" or "output". Other values might be possible in the future, should we add new layouts. 
    , percent :: !(Maybe Float) -- ^ The percentage which this container takes in its parent. A value of null means that the percent property does not make sense for this container, for example for the root container. 
    , rect :: !Rect -- ^ The absolute display coordinates for this container. Display coordinates means that when you have two 1600x1200 monitors on a single X11 Display (the standard way), the coordinates of the first window on the second monitor are { "x": 1600, "y": 0, "width": 1600, "height": 1200 }. 
    , window_rect :: !Rect -- ^ The coordinates of the actual client window inside its container. These coordinates are relative to the container and do not include the window decoration (which is actually rendered on the parent container). So, when using the default layout, you will have a 2 pixel border on each side, making the window_rect { "x": 2, "y": 0, "width": 632, "height": 366 } (for example). 
    , deco_rect :: !Rect -- ^ The coordinates of the window decoration inside its container. These coordinates are relative to the container and do not include the actual client window. 
    , geometry :: !Rect -- ^ The original geometry the window specified when i3 mapped it. Used when switching a window to floating mode, for example. 
    , window :: !(Maybe Int32) -- ^ The X11 window ID of the actual client window inside this container. This field is set to null for split containers or otherwise empty containers. This ID corresponds to what xwininfo(1) and other X11-related tools display (usually in hex). 
    , window_properties :: !Rect -- ^ X11 window properties title, instance, class, window_role and transient_for. 
    , urgent :: !Bool -- ^ Whether this container (window, split container, floating container or workspace) has the urgency hint set, directly or indirectly. All parent containers up until the workspace container will be marked urgent if they have at least one urgent child. 
    , focused :: !Bool -- ^ Whether this container is currently focused. 
    , focus :: !(Vector Int64) -- ^ List of child node IDs (see nodes, floating_nodes and id) in focus order. Traversing the tree by following the first entry in this array will result in eventually reaching the one node with focused set to true. 
    , nodes :: !(Vector Node) -- ^ The tiling (i.e. non-floating) child containers of this node. 
    , floating_nodes :: !(Vector Node) -- ^ The floating child containers of this node. Only non-empty on nodes with type workspace. 
} deriving (Eq, Generic, Show)

instance FromJSON Node where
    parseJSON = withObject "Node" $ \o -> do
        node_id                   <- o .: "id"
        name                 <- o .:? "name"
        nodetype             <- o .: "type"
        border               <- o .: "border"
        current_border_width <- o .: "current_border_width"
        layout               <- o .: "layout"
        percent              <- o .:? "percent"
        rect                 <- o .: "rect"
        window_rect          <- o .: "window_rect"
        deco_rect            <- o .: "deco_rect"
        geometry             <- o .: "geometry"
        window               <- o .:? "window"
        window_properties    <- o .: "window_properties"
        urgent               <- o .: "urgent"
        focused              <- o .: "focused"
        focus                <- o .: "focus"
        nodes                <- o .: "nodes"
        floating_nodes       <- o .: "floating_nodes"
        pure $! Node { .. }

data NodeBorder =
    Normal
    | None
    | Pixel
    deriving (Eq, Generic, Show)

instance ToJSON NodeBorder where
    toEncoding = \case
        Normal -> text "normal"
        None   -> text "none"
        Pixel  -> text "pixel"

instance FromJSON NodeBorder where
    parseJSON (String s) = pure $! case s of
        "normal" -> Normal
        "none"   -> None
        "pixel"  -> Pixel
        _        -> error "Unrecognized NodeBorder found"
    parseJSON _ = mzero

data Rect = Rect {
    x :: !Int32
    , y :: !Int32
    , width :: !Int32
    , height :: !Int32
} deriving (Eq, Generic, Show)

instance ToJSON Rect where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Rect

data NodeType =
    RootType
    | OutputType
    | ConType
    | FloatingConType
    | WorkspaceType
    | DockAreaType
    deriving (Eq, Generic, Show)

instance ToJSON NodeType where
    toEncoding = \case
        RootType        -> text "root"
        OutputType      -> text "output"
        ConType         -> text "con"
        FloatingConType -> text "floating_con"
        WorkspaceType   -> text "workspace"
        DockAreaType    -> text "dockarea"

instance FromJSON NodeType where
    parseJSON (String s) = pure $! case s of
        "root"         -> RootType
        "output"       -> OutputType
        "con"          -> ConType
        "floating_con" -> FloatingConType
        "workspace"    -> WorkspaceType
        "dockarea"     -> DockAreaType
        _              -> error "Received unrecognized NodeType"
    parseJSON _ = mzero

data NodeLayout =
    SplitHorizontalLayout
    | SplitVerticalLayout
    | StackedLayout
    | TabbedLayout
    | DockAreaLayout
    | OutputLayout
    deriving (Eq, Generic, Show)

instance ToJSON NodeLayout where
    toEncoding = \case
        SplitHorizontalLayout -> text "splith"
        SplitVerticalLayout   -> text "splitv"
        StackedLayout         -> text "stacked"
        TabbedLayout          -> text "tabbed"
        DockAreaLayout        -> text "dockarea"
        OutputLayout          -> text "output"

instance FromJSON NodeLayout where
    parseJSON (String s) = pure $! case s of
        "splith"   -> SplitHorizontalLayout
        "splitv"   -> SplitVerticalLayout
        "stacked"  -> StackedLayout
        "tabbed"   -> TabbedLayout
        "dockarea" -> DockAreaLayout
        "output"   -> OutputLayout
        _          -> error "Received unrecognized NodeLayout"
    parseJSON _ = mzero

-- | BarConfig Reply
