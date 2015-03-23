module Generation.ServiceGenerator(generateService) where

import qualified ApiSpec                     as AS
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, fromMaybe, isJust)
import qualified Data.Set                    as S
import qualified Generation.TemplateCompiler as TC

-- | Creates a Service object from a Specification object.
generateService :: AS.ApiSpec -> (AS.Type -> String) -> TC.Service
generateService apiSpec fieldMapping =
  TC.Service (AS.name apiSpec)
             (AS.version apiSpec)
             $ map (generateSchema fieldMapping apiSpec) (M.keys $ AS.resources apiSpec)

generateSchema :: (AS.Type -> String) -> AS.ApiSpec -> AS.Id -> TC.Schema
generateSchema fieldMapping apiSpec resId =
  TC.Schema { TC.schemaName = resId
            , TC.schemaRoute = schemaRoute'
            , TC.writable = writable'
            , TC.hasKeyField = hasKeyField
            , TC.keyField = keyField
            , TC.schemaVars = generateVars fieldMapping apiSpec structInfo }
  where
    (schemaRoute', writable') = fromJust $ M.lookup resId $ AS.resources apiSpec
    structInfo = fromJust $ M.lookup resId $ AS.structs apiSpec
    maybeKeyField = AS.getPrimaryKey structInfo
    keyField = fromMaybe ("" :: AS.Id) maybeKeyField
    hasKeyField = isJust maybeKeyField

generateVars :: (AS.Type -> String) -> AS.ApiSpec-> AS.StructInfo -> [TC.SchemaVar]
generateVars fieldMapping apiSpec = map getVarFromField
  where
    getVarFromField :: AS.FieldInfo -> TC.SchemaVar
    getVarFromField (AS.FI (n, t, modifs)) = generateSchemaVar n t modifs
    -- TODO(6): implement custom fields
    generateSchemaVar :: AS.Id -> AS.Type -> S.Set AS.Modifier -> TC.SchemaVar
    generateSchemaVar name type' modifs =
      TC.SchemaVar { TC.varName = name
                   , TC.varType = fieldMapping type'
                   , TC.isList = isList type'
                   , TC.isEnum = isEnum type'
                   , TC.enumValues = getValues type'
                   , TC.isStruct = isStruct type'
                   , TC.referredStruct = referredStruct type'
                   , TC.isKey = AS.PrimaryKey `S.member` modifs
                   , TC.isRequired = AS.Required `S.member` modifs }
        where
          getValues (AS.TEnum enumId) = map TC.EnumValue (fromJust $ M.lookup enumId $ AS.enums apiSpec)
          getValues _ = []
          isEnum (AS.TEnum _) = True
          isEnum _ = False
          isList (AS.TList _) = True
          isList _ = False
          isStruct (AS.TStruct _) = True
          isStruct _ = False
          referredStruct (AS.TStruct str) = str
          referredStruct _ = ""

