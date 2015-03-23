module LangUtilsSpec where

import           ApiSpec
import qualified Data.Map     as M
import           Language.Abs
import           LangUtils
import           Test.Hspec

spec :: Spec
spec =
  describe "LangUtilsSpec" $ do
    context "specName" $
      it "returns the spec name" $
        specName mockSpec `shouldBe` "name"

    context "specVersion" $
      it "returns the spec version" $
        specVersion mockSpec `shouldBe` "version"

    context "enumName" $
      it "returns the name of the enum" $
        enumName mockEnum `shouldBe` "EnumName"

    context "enumVals" $
      it "returns the list of values of an enum" $
        enumVals mockEnum `shouldBe` mockEnumVals

    context "enumValName" $
      it "returns the name of the value (unwraps it)" $
        enumValName (EnVal (Ident "ENUM_VAL_NAME")) `shouldBe` "ENUM_VAL_NAME"

    context "strName" $
      it "returns the name of the struct" $
        strName mockStruct `shouldBe` "StrName"

    context "strFields" $
      it "returns the fields of the struct" $
        strFields mockStruct `shouldBe` mockFields

    context "resName" $
      it "returns the name of a resource (which is the struct it refers)" $
        resName mockResource `shouldBe` "ResourceName"

    context "resRoute" $
      it "returns the route of a resource" $
        resRoute mockResource `shouldBe` "/route"

    context "resIsWritable" $
      it "returns the writing mode of a resource" $
        resIsWritable mockResource `shouldBe` True

    context "fieldName" $
      it "returns the name of a field from the AST" $
        fieldName mockField `shouldBe` "Field"

    context "fieldAnnotations" $
      it "returns the annotations of a field from the AST" $
        fieldAnnotations mockField `shouldBe` mockAnnotations

    context "fieldType" $
      it "returns the type of a field from the AST" $
        fieldType mockField `shouldBe` FString

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
    mockSpec = Spec (Nm (Ident "name")) (Ver (VerIdent "version")) [] [] []
    mockEnum = DefEnum (Ident "EnumName") mockEnumVals
    mockEnumVals = [EnVal (Ident "value1"), EnVal (Ident "value2")]
    mockStruct = DefStr (Ident "StrName") mockFields
    mockFields = [mockField]
    mockResource = Res (Ident "ResourceName") "/route" Write
    mockField = FDef mockAnnotations (Ident "Field") FString
    mockAnnotations = [Ann (Ident "PrimaryKey"), Ann (Ident "Hidden")]
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
