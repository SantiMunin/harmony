module LangUtilsSpec where

import qualified Data.Set          as S
import           Language.Abs
import           LangUtils
import           Test.Hspec
import           Test.QuickCheck
import           TestUtils         ()
import qualified TypeCheck.ApiSpec as AS

spec :: Spec
spec =
  describe "LangUtilsSpec" $ do
    context "specName" $
      it "returns the spec name" $ property $
        \spec@(Spec (Nm (Ident name)) _ _ _ _ _) -> specName spec == name

    context "specVersion" $
      it "returns the spec version" $ property $
        \spec@(Spec _ (Ver (VerIdent version)) _ _ _ _) -> specVersion spec == version

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
      it "returns ApiSpec.AS.TString from Language.Abs.TString" $
        fieldSpecType mockEnv FString `shouldBe` AS.TString

      it "returns ApiSpec.AS.TInt from Language.Abs.TInt" $
        fieldSpecType mockEnv FInt `shouldBe` AS.TInt

      it "returns ApiSpec.AS.AS.TDouble from Language.Abs.TDouble" $
        fieldSpecType mockEnv FDouble `shouldBe` AS.TDouble

      it "returns ApiSpec.AS.TEnum from a user defined enum" $
        fieldSpecType mockEnv (FDefined (Ident "AnEnum")) `shouldBe` AS.TEnum "AnEnum"

      it "returns ApiSpec.AS.TStruct from a user defined struct" $
        fieldSpecType mockEnv (FDefined (Ident "AStr")) `shouldBe` AS.TStruct "AStr"

      it "returns (ApiSpec.AS.TList (ApiSpec AS.TString)) from a FList FString" $
        fieldSpecType mockEnv (FList FString) `shouldBe` AS.TList AS.TString

      it "returns (ApiSpec.AS.TList (ApiSpec AS.TEnum)) from a list of user defined enum" $
        fieldSpecType mockEnv (FList (FDefined (Ident "AnEnum"))) `shouldBe` AS.TList (AS.TEnum "AnEnum")

  where
    mockEnv = (S.fromList ["AStr"], S.fromList ["AnEnum"])

main :: IO ()
main = hspec spec
