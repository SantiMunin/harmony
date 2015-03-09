module Generation.ServiceGenerator(generateService) where

import qualified ApiSpec                     as AS
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, fromMaybe)
import qualified Generation.TemplateCompiler as TC
import           LangUtils

-- | Creates a Service object from a Specification object.
generateService :: AS.ApiSpec -> (AS.Type -> String) -> TC.Service
generateService apiSpec fieldMapping =
  TC.Service (AS.name apiSpec)
             (AS.version apiSpec)
             $ map (generateSchema fieldMapping apiSpec) (M.keys $ AS.resources apiSpec)

generateSchema :: (AS.Type -> String) -> AS.ApiSpec -> AS.Id -> TC.Schema
generateSchema fieldMapping apiSpec resId =
  TC.Schema { TC.schemaName = resId
            , TC.schemaRoute = fromJust $ M.lookup resId $ AS.resources apiSpec
            , TC.keyField = AS.getPrimaryKey structInfo
            , TC.schemaVars = generateVars fieldMapping structInfo }
  where
    structInfo = fromJust $ M.lookup resId $ AS.structs apiSpec

generateVars :: (AS.Type -> String) -> AS.StructInfo -> [TC.SchemaVar]
generateVars fieldMapping = map getVarFromField
  where
    getVarFromField :: AS.FieldInfo -> TC.SchemaVar
    getVarFromField (n, t, modifs) = generateSchemaVar n t modifs
    -- TODO(6): implement custom fields
    generateSchemaVar :: AS.Id -> AS.Type -> [AS.Modifier] -> TC.SchemaVar
    generateSchemaVar name type' modifs =
      TC.SchemaVar { TC.varName = name
                   , TC.varType = fieldMapping type'
                   , TC.isKey = AS.PrimaryKey `elem` modifs
                   , TC.isRequired = AS.Required `elem` modifs }

