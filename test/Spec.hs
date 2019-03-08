import           Test.Hspec
import qualified Data.ByteString.Lazy               as BL
import           Data.Aeson                          ( decode )
import           Data.Maybe                          ( isJust )

import qualified I3IPC.Event                        as Evt
import qualified I3IPC.Reply as Reply

main :: IO ()
main = hspec $ do
    describe "Events" $ do
        it "Deserialize WindowEvent" $ do
            f <- BL.readFile "./test/event/winevent.json"
            let w = decode f :: Maybe Evt.WindowEvent
            isJust w `shouldBe` True
        it "Deserialize WorkspaceEvent" $ do
            f <- BL.readFile "./test/event/workspace.json"
            let w = decode f :: Maybe Evt.WorkspaceEvent
            isJust w `shouldBe` True
    describe "Replies" $ do 
        it "Deserialise OutputReply" $ do 
            f <- BL.readFile "./test/reply/output.json"
            let o = decode f :: Maybe Reply.OutputsReply
            isJust o `shouldBe` True
        it "Deserialise TreeReply" $ do 
            f <- BL.readFile "./test/reply/tree.json"
            let o = decode f :: Maybe Reply.Node
            isJust o `shouldBe` True
        it "Deserialise VersionReply" $ do 
            f <- BL.readFile "./test/reply/version.json"
            let o = decode f :: Maybe Reply.VersionReply
            isJust o `shouldBe` True
        it "Deserialise WorkspaceReply" $ do 
            f <- BL.readFile "./test/reply/workspace.json"
            let o = decode f :: Maybe Reply.WorkspaceReply
            isJust o `shouldBe` True