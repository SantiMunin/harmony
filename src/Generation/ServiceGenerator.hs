module Generation.ServiceGenerator(generateService) where

import           Generation.TemplateCompiler
import           Language.Abs
import           LangUtils

-- | Creates a Service object from a Specification object.
generateService :: Specification -> (Field -> String) -> Service
generateService (Spec (Nm (Ident name)) (Ver (VerIdent version)) _ structs resources) fieldMapping =
  Service name version $ map (generateSchema fieldMapping structs) resources

generateSchema :: (Field -> String) -> [StructType] -> Resource -> Schema
generateSchema fieldMapping structs (DefResNoOp (Ident name') route' _) =
  Schema { schemaName = name'
         , schemaRoute = route'
         , keyField = getKeyField struct
         , schemaVars = generateVars fieldMapping struct }
  where
    struct = head $ filter (\str -> strName str == name') structs
    getKeyField :: StructType -> String
    getKeyField (DefStr _ fields) = fieldName $ head $ filter (isPk . fieldAnnotations) fields

generateVars :: (Field -> String) -> StructType -> [SchemaVar]
generateVars fieldMapping (DefStr _ fields) = map getVarFromField fields
  where
    getVarFromField field = generateSchemaVar (fieldName field) (fieldMapping field) (fieldAnnotations field)
    -- TODO(6): implement custom fields
    generateSchemaVar :: String -> String -> [Annotation] -> SchemaVar
    generateSchemaVar name type' annotations =
      SchemaVar { varName = name
                , varType = type'
                , isKey = isPk annotations
                , isRequired = isRequiredField annotations }

containsAnnotation :: String -> [Annotation] -> Bool
containsAnnotation name = any (\(Ann (Ident annName)) -> annName == name)

isPk :: [Annotation] -> Bool
isPk = containsAnnotation "PK"

isRequiredField :: [Annotation] -> Bool
isRequiredField = containsAnnotation "Required"

