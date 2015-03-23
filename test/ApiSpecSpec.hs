module ApiSpecSpec where

import           ApiSpec
import           Test.Hspec

spec :: Spec
spec =
  describe "ApiSpec" $
    context "getPrimaryKey" $ do
      it "returns the primary key of a struct" $
        getPrimaryKey [("regularField", TString, [Immutable])
                     , ("pkField", TInt, [PrimaryKey, Immutable])] `shouldBe` Just "pkField"
      it "returns nothing if empty struct" $
        getPrimaryKey [] `shouldBe` Nothing
      it "returns nothing if there is no field with the PrimaryKey modifier" $
        getPrimaryKey [("regularField", TString, [Immutable])] `shouldBe` Nothing

main :: IO ()
main = hspec spec
