-- | Generation of 'TC.Service' objects from the specification.
module Generation.ServiceGenerator(generateService) where

import qualified ApiSpec                     as AS
import qualified Data.Map                    as M
import           Data.Maybe                  (fromJust, fromMaybe, isJust)
import qualified Data.Set                    as S
import qualified Generation.TemplateCompiler as TC

-- | Transforms an api specification to a service.
generateService :: AS.ApiSpec -- ^ The specification of the web service
                -> (AS.Type -> String) -- ^ A mapping from internal types to target's types
                -> TC.Service
generateService apiSpec fieldMapping =
  TC.Service (AS.name apiSpec)
             (AS.version apiSpec)
             (AS.requiresAuth apiSpec)
             $ map (generateSchema fieldMapping apiSpec . fst) $ AS.structs apiSpec

-- | Generates the information of a resource/struct.
generateSchema :: (AS.Type -> String) -- ^ A mapping from internal types to target's types
               -> AS.ApiSpec -- ^ The specification of the web service
               -> AS.Id -- ^ The name of the struct
               -> TC.Schema
generateSchema fieldMapping apiSpec strId =
  TC.Schema { TC.schemaName = strId
            , TC.schemaRoute = schemaRoute'
            , TC.writable = writable'
            , TC.hasKeyField = hasKeyField
            , TC.keyField = keyField
            , TC.schemaVars = generateVars fieldMapping apiSpec structInfo }
  where
    (schemaRoute', writable') = maybe (Nothing, False) (\(r, w) -> (Just TC.StrValue { TC.value = r }, w)) (M.lookup strId $ AS.resources apiSpec)
    structInfo = fromJust $ lookup strId $ AS.structs apiSpec
    maybeKeyField = AS.getPrimaryKey structInfo
    -- Leave empty if it doesn't exist
    keyField = fromMaybe "" maybeKeyField
    hasKeyField = isJust maybeKeyField

-- | Generates the information of the fields of a 'TC.Schema'.
generateVars :: (AS.Type -> String) -- ^ A mapping from internal types to target's types
             -> AS.ApiSpec -- ^ The specification of the web service
             -> AS.StructInfo -- ^ The information of the struct
             -> [TC.SchemaVar] -- ^ The information of all the fields of the schema
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
                   , TC.isEnum = if isEnum type' then Just $ getValues type' else Nothing
                   , TC.isStruct = isStruct type'
                   , TC.isKey = AS.PrimaryKey `S.member` modifs
                   , TC.isRequired = AS.Required `S.member` modifs
                   , TC.isHidden = AS.Hidden `S.member` modifs
                   , TC.isUnique = AS.Unique `S.member` modifs || AS.PrimaryKey `S.member` modifs
                   , TC.isUserLogin = AS.UserLogin `S.member` modifs
                   }
        where
          getValues (AS.TEnum enumId) = TC.EnumValue $ map TC.StrValue (fromJust $ M.lookup enumId $ AS.enums apiSpec)
          getValues (AS.TList t') = getValues t'
          getValues other = error $ "getValues called on a non-enum type: " ++ show other
          isEnum (AS.TEnum _) = True
          isEnum (AS.TList t') = isEnum t'
          isEnum _ = False
          isList (AS.TList _) = True
          isList _ = False
          isStruct (AS.TStruct _) = True
          isStruct (AS.TList t') = isStruct t'
          isStruct _ = False

