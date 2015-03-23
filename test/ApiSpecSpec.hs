module ApiSpecSpec where

import           ApiSpec    as AS
import qualified Data.Set   as S
import           Test.Hspec

spec :: Spec
spec =
  describe "ApiSpec" $
    context "getPrimaryKey" $ do
      it "returns the primary key of a struct" $
        getPrimaryKey [AS.FI ("regularField", TString, S.fromList [Immutable])
                     , AS.FI ("pkField", TInt, S.fromList [PrimaryKey, Immutable])] `shouldBe` Just "pkField"
      it "returns nothing if empty struct" $
        getPrimaryKey [] `shouldBe` Nothing
      it "returns nothing if there is no field with the PrimaryKey modifier" $
        getPrimaryKey [AS.FI ("regularField", TString, S.fromList [Immutable])] `shouldBe` Nothing

main :: IO ()
main = hspec spec
