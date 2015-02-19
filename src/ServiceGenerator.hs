module ServiceGenerator(generateService) where

import           Language.Abs
import           TemplateCompiler

-- | Creates a Service object from a Specification object.
generateService :: Specification -> Service
-- TODO: implement
generateService (Spec name version enums structs resources) = Service $ map (generateSchema structs) resources

--Service [Schema { name = "Map", route = "route", keyField = "key", schemaVars = [SchemaVar { varName = "key", varType = "String", isKey = False, isRequired = False}]}]

generateSchema :: [StructType] -> Resource -> Schema
generateSchema structs (DefResNoOp (Ident name') route' _) =
  Schema { name = name'
         , route = route'
         , keyField = getKeyField struct
         , schemaVars = generateVars struct }
  where
    struct = head $ filter (\(DefStr (Ident strName) _) -> strName == name') structs
    getKeyField :: StructType -> String
    getKeyField (DefStr _ fields) = extractName $ head $ filter hasPkAnnotation fields
    hasPkAnnotation (FString annotations (Ident name)) = isPk annotations
    hasPkAnnotation (FInt annotations (Ident name)) = isPk annotations
    hasPkAnnotation (FDouble annotations (Ident name)) = isPk annotations
    hasPkAnnotation _ = error "hasPkAnnotation not implemented yet for custom types"

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

extractName :: Field -> String
extractName (FString _ (Ident name)) = name
extractName (FInt _ (Ident name)) = name
extractName (FDouble _ (Ident name)) = name
extractName _ = error "Extract name for custom types not implemented yet"
