# i3ipc

[![Build Status](https://travis-ci.com/leshow/i3ipc.svg?branch=master)](https://travis-ci.com/leshow/i3ipc)

Haskell type-safe bindings for working with i3 using it's unix socket IPC

Subscribe to events:

```haskell
import qualified I3IPC.Subscribe   as Sub
import           I3IPC              ( subscribe )

-- will print all events
main :: IO ()
main = subscribe print [Sub.Workspace, Sub.Window]
```

Sending Messages to i3:

```haskell
import           I3IPC              ( connecti3
                                    , getWorkspaces
                                    )

main :: IO ()
main = do
    soc <- connecti3
    print getWorkspaces
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

I'm happy to take PRs or suggestions, or simply fix issues for this library.
