module LangUtilsSpec where

import           ApiSpec
import qualified Data.Map        as M
import           Language.Abs
import           LangUtils
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils       ()

spec :: Spec
spec =
  describe "LangUtilsSpec" $ do
    context "specName" $
      it "returns the spec name" $ property $
        \spec@(Spec (Nm (Ident name)) _ _ _ _) -> specName spec == name

    context "specVersion" $
      it "returns the spec version" $ property $
        \spec@(Spec _ (Ver (VerIdent version)) _ _ _) -> specVersion spec == version

    context "enumName" $
      it "returns the name of the enum" $ property $
        \enum@(DefEnum (Ident name) _ ) -> enumName enum == name

    context "enumVals" $
      it "returns the list of values of an enum" $ property $
        \enum@(DefEnum _ vals) -> enumVals enum == vals

    context "enumValName" $
      it "returns the name of the value (unwraps it)" $ property $
        \enumVal@(EnVal (Ident valName)) -> enumValName enumVal == valName

    context "strName" $
      it "returns the name of the struct" $ property $
        \str@(DefStr (Ident name) _ ) -> strName str == name

    context "strFields" $
      it "returns the fields of the struct" $ property $
        \str@(DefStr _ fields) -> strFields str == fields

    context "resName" $
      it "returns the name of a resource (which is the struct it refers)" $ property $
        \res@(Res (Ident name) _ _) -> resName res == name

    context "resRoute" $
      it "returns the route of a resource" $ property $
        \res@(Res _ (RouteIdent route) _) -> resRoute res == route

    context "resIsWritable" $
      it "returns the writing mode of a resource" $ property $
        \res@(Res _ _ mode) -> resIsWritable res == (mode == Write)

    context "fieldName" $
      it "returns the name of a field from the AST" $ property $
        \field@(FDef _ (Ident name) _) -> fieldName field == name

    context "fieldAnnotations" $
      it "returns the annotations of a field from the AST" $ property $
        \field@(FDef ann _ _) -> fieldAnnotations field == ann

    context "fieldType" $
      it "returns the type of a field from the AST" $ property $
        \field@(FDef _ _ ftype) -> fieldType field == ftype

    context "fieldSpecType" $ do
      it "returns ApiSpec.TString from Language.Abs.TString" $
        fieldSpecType mockApiSpec FString `shouldBe` TString

      it "returns ApiSpec.TInt from Language.Abs.TInt" $
        fieldSpecType mockApiSpec FInt `shouldBe` TInt

      it "returns ApiSpec.TDouble from Language.Abs.TDouble" $
        fieldSpecType mockApiSpec FDouble `shouldBe` TDouble

      it "returns ApiSpec.TEnum from a user defined enum" $
        fieldSpecType mockApiSpec (FDefined (Ident "AnEnum")) `shouldBe` TEnum "AnEnum"

      it "returns ApiSpec.TStruct from a user defined struct" $
        fieldSpecType mockApiSpec (FDefined (Ident "AStr")) `shouldBe` TStruct "AStr"

      it "returns (ApiSpec.TList (ApiSpec TString)) from a FList FString" $
        fieldSpecType mockApiSpec (FList FString) `shouldBe` TList TString

      it "returns (ApiSpec.TList (ApiSpec TEnum)) from a list of user defined enum" $
        fieldSpecType mockApiSpec (FList (FDefined (Ident "AnEnum"))) `shouldBe` TList (TEnum "AnEnum")

  where
    mockApiSpec = AS { name = "name"
                     , version = "version"
                     , enums = M.insert "AnEnum" mockEnumInfo M.empty
                     , structs = M.insert "AStr" mockStructInfo M.empty
                     , resources = M.empty
                     }
    mockEnumInfo = []
    mockStructInfo = []


main :: IO ()
main = hspec spec
