{-# LANGUAGE FlexibleInstances #-}
module Generation.ServiceGeneratorSpec where

import qualified ApiSpec                     as AS
import qualified Data.Map                    as M
import           Generation.ServiceGenerator
import qualified Generation.TemplateCompiler as TC
import           Test.Hspec
import           Test.QuickCheck
import qualified Test.QuickCheck.Property    as P
import           Text.Show.Functions         ()

spec :: Spec
spec =
  describe "Generator.ServiceGeneratorSpec" $
    context "generateService" $ do

      it "translates correctly (name, version) from ApiSpec to Service" $ property $
        \apiSpec fieldMapping -> let result = generateService apiSpec fieldMapping in
                                   (TC.name result == AS.name apiSpec) && (TC.version result == AS.version apiSpec)

      it "translates correctly the enums from ApiSpec to Service" $ property prop_enumsCorrectlyTranslated

      it "translates correctly the structs from ApiSpec to Service" $ property prop_structsCorrectlyTranslated

      it "handles the (optional) primary key field correctly" $ property prop_handlesPrimaryKeyCorrectly

      return ()

prop_enumsCorrectlyTranslated :: AS.ApiSpec -> Property
prop_enumsCorrectlyTranslated apiSpec =
  P.printTestCase ("\nResult of generateService: " ++  show result) (property $ all enumProcessingCorrect $ allVariables result)
  where
    result = generateService apiSpec fieldMapping
    apiSpecEnumInfo = AS.enums apiSpec
    enumProcessingCorrect schemaVar =
      if TC.isEnum schemaVar
      then varInSpecEnums schemaVar && not (null $ TC.enumValues schemaVar)
      else null $ TC.enumValues schemaVar
    varInSpecEnums var = TC.varType var `M.member` apiSpecEnumInfo

prop_structsCorrectlyTranslated :: AS.ApiSpec -> Property
prop_structsCorrectlyTranslated apiSpec =
  P.printTestCase ("\nResult of generateService: " ++ show result) (property $ all structProcessingCorrect $ allVariables result)
  where
    result = generateService apiSpec fieldMapping
    apiSpecStructInfo = AS.structs apiSpec
    structProcessingCorrect schemaVar =
      if TC.isStruct schemaVar
      then varInSpecStructs schemaVar
      else not $ varInSpecStructs schemaVar
    varInSpecStructs var = TC.varType var `M.member` apiSpecStructInfo

prop_handlesPrimaryKeyCorrectly :: AS.ApiSpec -> Property
prop_handlesPrimaryKeyCorrectly apiSpec =
  P.printTestCase ("\nResult of generateService: " ++ show result) (property $ all primaryKeyProcessingCorrect $ allVariables result)
  where
    result = generateService apiSpec fieldMapping
    primaryKeyProcessingCorrect schemaVar =
      if TC.isKey schemaVar
      then assertIsKey schemaVar
      else not $ assertIsKey schemaVar
    assertIsKey schemaVar = any (\s -> TC.hasKeyField s
                                    && TC.keyField s == TC.varName schemaVar
                                    && schemaVar `elem` TC.schemaVars s ) $ TC.schema result

allVariables :: TC.Service -> [TC.SchemaVar]
allVariables result = concatMap TC.schemaVars $ TC.schema result

fieldMapping :: AS.Type -> String
fieldMapping AS.TString = "String"
fieldMapping AS.TInt = "Int"
fieldMapping AS.TLong = "Long"
fieldMapping AS.TFloat = "Float"
fieldMapping AS.TDouble = "Double"
fieldMapping AS.TBool = "Bool"
fieldMapping (AS.TEnum t) = t
fieldMapping (AS.TStruct t) = t
fieldMapping (AS.TList t) = fieldMapping t

main :: IO ()
main = hspec spec
