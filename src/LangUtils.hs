-- | Contains useful methods to work with the defined types (both from the language
-- specification/bnfc and the 'ApiSpec' module).
module LangUtils where

import qualified ApiSpec      as AS
import qualified Data.Map     as M
import           Language.Abs

-- | Extracts the name of the 'Specification' (AST).
specName :: Specification -- ^ Specification returned by the parser
         -> String -- ^ Name of the specification
specName (Spec (Nm (Ident n)) _ _ _ _) = n

-- | Extracts the version of the 'Specification' (AST).
specVersion :: Specification -- ^ Specification returned by the parser
            -> String -- ^ Version of the specification
specVersion (Spec _(Ver (VerIdent v)) _ _ _) = v

-- | Extracts the name of an enum.
enumName :: EnumType -- ^ Enum information
         -> String -- ^ Name of the enum type
enumName (DefEnum (Ident name) _) = name

-- | Extracts the values of an enum.
enumVals :: EnumType -- ^ Enum information
         -> [EnumVal] -- ^ List of enum values
enumVals (DefEnum _ vals) = vals

enumValName :: EnumVal -- ^ Enum value information
            -> String -- ^ Identifier of the enum value
enumValName (EnVal (Ident name)) = name

-- | Extracts the name of a struct.
strName :: StructType -- ^ The struct info
        -> String -- ^ The name of the struct
strName (DefStr (Ident name) _) = name

-- | Extracts the fields of a struct.
strFields :: StructType -- ^ Struct info
          -> [Field] -- ^ List of fields of the struct
strFields (DefStr _ fields) = fields

-- | Extracts the name of a resource.
resName :: Resource -- ^ Resource information
        -> String -- ^ Name of the resource
resName (Res (Ident name) _ _) = name

-- | Extracts the route of a resource.
resRoute :: Resource -- ^ Resource information
         -> String -- ^ Route of the resource
resRoute (Res _ route _) = route

-- | Extracts the write mode of a resource.
resIsWritable :: Resource -- ^ Resource information
              -> Bool -- ^ True if it is writable, False otherwise
resIsWritable (Res _ _ mode) = mode == Write

-- | Extracts the name of a field.
fieldName :: Field -- ^ Field information
          -> String -- ^ Name of the field
fieldName (FDef _ (Ident name) _) = name

-- | Extracts the annotation of a field.
fieldAnnotations :: Field -- ^ Field information
                 -> [Annotation] -- ^ List of annotations
fieldAnnotations (FDef annotations _ _) = annotations

-- | Extracts the type (defined by the language specification/bnfc) of a field.
fieldType :: Field -- ^ Field information
          -> FType -- ^ Type of the field
fieldType (FDef _ _ ft) = ft

-- | Extracts the type (defined in 'ApiSpec') of a field.
fieldSpecType :: AS.ApiSpec -- ^ Specification of the service
              -> FType -- ^ Field type
              -> AS.Type -- ^ 'ApiSpec' field type
fieldSpecType _ FString = AS.TString
fieldSpecType _ FInt = AS.TInt
fieldSpecType _ FDouble = AS.TDouble
fieldSpecType as (FDefined (Ident name)) = getType as name
  where
    getType env t | t `M.member` AS.enums env = AS.TEnum t
                  | t `M.member` AS.structs env = AS.TStruct t
                  | otherwise = error $ "getType: " ++ t ++ " is not defined."
fieldSpecType as (FList type') = AS.TList $ fieldSpecType as type'
