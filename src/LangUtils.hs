module LangUtils where

import qualified ApiSpec      as AS
import qualified Data.Map     as M
import           Language.Abs

specName :: Specification -> String
specName (Spec (Nm (Ident n)) _ _ _ _) = n

specVersion :: Specification -> String
specVersion (Spec _(Ver (VerIdent v)) _ _ _) = v

enumName :: EnumType -> String
enumName (DefEnum (Ident name) _) = name

enumVals :: EnumType -> [EnumVal]
enumVals (DefEnum _ vals) = vals

enumValName :: EnumVal -> String
enumValName (EnVal (Ident name)) = name

strName :: StructType -> String
strName (DefStr (Ident name) _) = name

strFields :: StructType -> [Field]
strFields (DefStr _ fields) = fields

resName :: Resource -> String
resName (Res (Ident name) _ _) = name

resRoute :: Resource -> String
resRoute (Res _ route _) = route

resIsWritable :: Resource -> Bool
resIsWritable (Res _ _ mode) = mode == Write

fieldName :: Field -> String
fieldName (FDef _ (Ident name) _) = name

fieldAnnotations :: Field -> [Annotation]
fieldAnnotations (FDef annotations _ _) = annotations

fieldToType :: Field -> FType
fieldToType (FDef _ _ ft) = ft

fieldToSpecType :: AS.ApiSpec -> FType -> AS.Type
fieldToSpecType _ FString = AS.TString
fieldToSpecType _ FInt = AS.TInt
fieldToSpecType _ FDouble = AS.TDouble
fieldToSpecType as (FDefined (Ident name)) = getType as name
  where
    getType env t | t `M.member` AS.enums env = AS.TEnum t
                  | t `M.member` AS.structs env = AS.TStruct t
                  | otherwise = error $ "getType: " ++ t ++ " is not defined."
fieldToSpecType as (FList type') = AS.TList $ fieldToSpecType as type'
