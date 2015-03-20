module Generation.ServiceGenerator(generateService) where

import qualified ApiSpec                     as AS
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, fromMaybe, isJust)
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
    getVarFromField (n, t, modifs) = generateSchemaVar n t modifs
    -- TODO(6): implement custom fields
    generateSchemaVar :: AS.Id -> AS.Type -> [AS.Modifier] -> TC.SchemaVar
    generateSchemaVar name type' modifs =
      TC.SchemaVar { TC.varName = name
                   , TC.varType = fieldMapping type'
                   , TC.isEnum = isEnum type'
                   , TC.enumValues = getValues type'
                   , TC.isKey = AS.PrimaryKey `elem` modifs
                   , TC.isRequired = AS.Required `elem` modifs }
        where
          getValues (AS.TEnum enumId) = map TC.EnumValue (fromJust $ M.lookup enumId $ AS.enums apiSpec)
          getValues _ = []
          isEnum (AS.TEnum _) = True
          isEnum _ = False

