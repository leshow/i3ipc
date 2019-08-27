# i3ipc

[![Build Status](https://travis-ci.com/leshow/i3ipc.svg?branch=master)](https://travis-ci.com/leshow/i3ipc)

Haskell type-safe bindings for working with i3 using it's unix socket IPC

## Subscribing

Subscribe to events:

```haskell
import qualified I3IPC.Subscribe   as Sub
import           I3IPC              ( subscribe )

-- will print all events
main :: IO ()
main = subscribe print [Sub.Workspace, Sub.Window]
```

An example of explicitly matching on some events and printing their fields:

```haskell
import qualified I3IPC.Subscribe               as Sub
import           I3IPC.Event
import           I3IPC                          ( subscribe )

main :: IO ()
main = subscribe handle [Sub.Workspace, Sub.Window]
 where
  handle :: Either String Event -> IO ()
  handle (Right evt) = case evt of
    Workspace WorkspaceEvent { wrk_current } -> print wrk_current
    Window WindowEvent { win_container } -> print win_container
    _ -> error "No other event types"
  handle (Left err) = error err
```

## Sending Messages

Sending Messages to i3:

```haskell
import           I3IPC              ( connecti3
                                    , getWorkspaces
                                    )

main :: IO ()
main = do
    soc <- connecti3
    print $ getWorkspaces soc
```

Alternatively, you can ignore the convenience functions and construct these messages yourself:

```haskell
import qualified I3IPC.Message     as Msg
import           I3IPC              ( connecti3
                                    , receiveMsg
                                    )

main :: IO ()
main = do
    soc <- connecti3
    print $ Msg.sendMsg soc Msg.Workspaces >> receiveMsg soc
```

## Community

I'm happy to take PRs or suggestions, or simply fix issues for this library.
