module TypeCheck.ApiSpecSpec where

import           Control.Exception
import qualified Data.Set          as S
import qualified Data.Map          as M
import           Test.Hspec
import           TestUtils
import           TypeCheck.ApiSpec as AS
import TypeCheck.StructTrack

spec :: Spec
spec =
  describe "ApiSpec" $ do
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
    context "StruckTrack" $ do
      it "adds a reference to a struct that didn't exist" $
        M.lookup "a" (addStructReference newStructTrack "a" "b") `shouldBe` Just ["b"]

      it "adds a reference to a struct that existed" $
        M.lookup "a" (addStructReference (addStructReference newStructTrack "a" "b") "a" "c") `shouldBe` Just ["c", "b"]


main :: IO ()
main = hspec spec
