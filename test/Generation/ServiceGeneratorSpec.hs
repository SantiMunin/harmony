{-# LANGUAGE FlexibleInstances #-}
module Generation.ServiceGeneratorSpec where

import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, isJust, isNothing)
import           Generation.ServiceGenerator
import qualified Generation.TemplateCompiler as TC
import           Test.Hspec
import           Test.QuickCheck
import qualified Test.QuickCheck.Property    as P
import           Text.Show.Functions         ()
import qualified TypeCheck.ApiSpec           as AS

spec :: Spec
spec =
  describe "Generator.ServiceGeneratorSpec" $
    context "generateService" $ do

      it "translates correctly (name, version) from ApiSpec to Service" $ property $
        \apiSpec fieldMapping -> let result = generateService apiSpec fieldMapping boxedFieldMapping in
                                   (TC.name result == AS.name apiSpec) && (TC.version result == AS.version apiSpec)

      it "translates correctly the enums from ApiSpec to Service" $ property prop_enumsCorrectlyTranslated

      it "translates correctly the structs from ApiSpec to Service" $ property prop_structsCorrectlyTranslated

      it "handles the (optional) primary key field correctly" $ property prop_handlesPrimaryKeyCorrectly

      return ()

prop_enumsCorrectlyTranslated :: AS.ApiSpec -> Property
prop_enumsCorrectlyTranslated apiSpec =
  P.counterexample ("\nResult of generateService: " ++  show result) (property $ all enumProcessingCorrect $ allVariables result)
  where
    result = generateService apiSpec fieldMapping boxedFieldMapping
    apiSpecEnumInfo = AS.enums apiSpec
    -- It is either not an enum or it has some values and the enum exists.
    enumProcessingCorrect schemaVar =
      isNothing (TC.isEnum schemaVar) ||
          (varInSpecEnums schemaVar &&
             not (null $ TC.values $ fromJust $ TC.isEnum schemaVar))
    varInSpecEnums var = TC.varType var `M.member` apiSpecEnumInfo

prop_structsCorrectlyTranslated :: AS.ApiSpec -> Property
prop_structsCorrectlyTranslated apiSpec =
  P.counterexample ("\nResult of generateService: " ++ show result) (property $ all structProcessingCorrect $ allVariables result)
  where
    result = generateService apiSpec fieldMapping boxedFieldMapping
    apiSpecStructInfo = AS.structs apiSpec
    structProcessingCorrect schemaVar =
      if TC.isStruct schemaVar
      then varInSpecStructs schemaVar
      else not $ varInSpecStructs schemaVar
    varInSpecStructs var = isJust $ lookup (TC.varType var) apiSpecStructInfo

prop_handlesPrimaryKeyCorrectly :: AS.ApiSpec -> Property
prop_handlesPrimaryKeyCorrectly apiSpec =
  P.counterexample ("\nResult of generateService: " ++ show result) (property $ all primaryKeyProcessingCorrect $ allVariables result)
  where
    result = generateService apiSpec fieldMapping boxedFieldMapping
    primaryKeyProcessingCorrect schemaVar =
      if TC.isKey schemaVar
      then assertIsKey schemaVar
      else not $ assertIsKey schemaVar
    assertIsKey schemaVar = any (\s -> TC.hasKeyField s
                                    && TC.keyField s == TC.varName schemaVar
                                    && schemaVar `elem` TC.schemaVars s
                                    && TC.isUnique schemaVar) $ TC.schema result

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

-- Re use fieldMapping, we don't care about the concrete values for these tests.
boxedFieldMapping :: AS.Type -> String
boxedFieldMapping = fieldMapping

main :: IO ()
main = hspec spec
