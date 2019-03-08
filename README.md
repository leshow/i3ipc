# i3ipc

Haskell type-safe bindings for working with i3 using it's unix socket IPC

Subscribe to events:

```haskell
import qualified I3IPC.Subscribe   as Sub
import           I3IPC              ( subscribe )

-- will print all events
main :: IO ()
main = subscribe print [Sub.Workspace, Sub.Window]
```
