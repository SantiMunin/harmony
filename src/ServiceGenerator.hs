module ServiceGenerator(generateService) where

import           Language.Abs
import           LangUtils
import           TemplateCompiler
-- | Creates a Service object from a Specification object.
generateService :: Specification -> Service
generateService (Spec name version enums structs resources) = Service $ map (generateSchema structs) resources

generateSchema :: [StructType] -> Resource -> Schema
generateSchema structs (DefResNoOp (Ident name') route' _) =
  Schema { name = name'
         , route = route'
         , keyField = getKeyField struct
         , schemaVars = generateVars struct }
  where
    struct = head $ filter (\str -> strName str == name') structs
    getKeyField :: StructType -> String
    getKeyField (DefStr _ fields) = fieldName $ head $ filter (isPk . fieldAnnotations) fields

generateVars :: StructType -> [SchemaVar]
generateVars (DefStr (Ident name) fields) = map getVarFromField fields
  where
    getVarFromField (FString annotations (Ident name)) = generateSchemaVar name "String" annotations
    getVarFromField (FInt annotations (Ident name)) = generateSchemaVar name "Number" annotations
    getVarFromField (FDouble annotations (Ident name)) = generateSchemaVar name "Number" annotations
    -- TODO: implement
    getVarFromField _ = undefined
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

