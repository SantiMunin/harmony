module LangUtils where

import           Language.Abs

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
resName (DefResNoOp (Ident name) _ _) = name

resRoute :: Resource -> String
resRoute (DefResNoOp _ route _) = route

fieldName :: Field -> String
fieldName (FString _ (Ident name)) = name
fieldName (FInt _ (Ident name)) = name
fieldName (FDouble _ (Ident name)) = name
fieldName (FDefined _ (Ident name) _) = name

fieldAnnotations :: Field -> [Annotation]
fieldAnnotations (FString annotations _) = annotations
fieldAnnotations (FInt annotations _) = annotations
fieldAnnotations (FDouble annotations _) = annotations
fieldAnnotations (FDefined annotations _ _) = annotations

fieldType :: Field -> String
fieldType (FString _ _) = "String"
fieldType (FInt _ _) = "Int"
fieldType (FDouble _ _) = "Double"
fieldType (FDefined _ _ (Ident typeName)) = typeName
