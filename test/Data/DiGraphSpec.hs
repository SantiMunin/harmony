module Data.DiGraphSpec where

import           Data.DiGraph
import           Test.Hspec

spec :: Spec
spec =
  describe "Data.DiGraphSpec" $
    context "hasCycle" $ do

      it "returns false for an empty graph" $
        hasCycle (empty :: Graph String) `shouldBe` False

      it "returns false for a one-node graph" $
        hasCycle (create [("v", [])]) `shouldBe` False

      it "returns false for a list-like graph with no repeated elements" $
        hasCycle (create [("a", ["b"]), ("b", ["c"])]) `shouldBe` False

      it "returns true for a list-like graph with repeated elements" $
        hasCycle (create [("a", ["b"]), ("b", ["c"]), ("c", ["a"])]) `shouldBe` True

      it "returns false for a binary tree with no repeated elements" $
        hasCycle (create [("a", ["a1", "a2"]), ("a1", ["b1", "b2"]), ("a2", ["b3", "b4"])]) `shouldBe` False

      it "returns true for a binary tree with repeated elements" $
        hasCycle (create [("a", ["a1", "a2"]), ("a1", ["b1", "b2"]), ("a2", ["b3", "b4"]), ("b3", ["c1", "a2"])]) `shouldBe` True


main :: IO ()
main = hspec spec
