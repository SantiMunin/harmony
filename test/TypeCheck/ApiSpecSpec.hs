module TypeCheck.ApiSpecSpec where

import           Control.Exception
import qualified Data.Set          as S
import           Test.Hspec
import           TestUtils
import           TypeCheck.ApiSpec as AS

spec :: Spec
spec =
  describe "ApiSpec" $
    context "getPrimaryKey" $ do
      it "returns nothing if empty struct" $
        getPrimaryKey [] `shouldBe` Nothing

      it "returns nothing if there is no field with the PrimaryKey modifier" $
        getPrimaryKey [AS.FI ("regularField", TString, S.fromList [Immutable])] `shouldBe` Nothing

      it "returns the primary key of a struct" $
        getPrimaryKey [AS.FI ("regularField", TString, S.fromList [Immutable])
                     , AS.FI ("pkField", TInt, S.fromList [PrimaryKey, Immutable])] `shouldBe` Just "pkField"

      it "throws an error if there is more than one field with a PrimaryKey modifier" $
        assertException (ErrorCall "A struct should have at most one specified primary key.") $
          getPrimaryKey [AS.FI ("regularField", TString, S.fromList [PrimaryKey])
                        , AS.FI ("pkField", TInt, S.fromList [PrimaryKey, Immutable])] `shouldBe` Just "pkField"

main :: IO ()
main = hspec spec
