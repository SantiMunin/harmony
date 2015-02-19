module StaticCheck (staticCheck) where

import           Control.Monad (unless)
import           Data.Char
import qualified Data.Foldable as F
import qualified Data.Set      as S

import           Language.Abs
import           Language.ErrM

-- | Checks the definition for:
--   * Name clashes
--   * Undefined types
staticCheck :: Specification -> Err Specification
staticCheck spec@(Spec _ _ enums structs resources) =
     checkClashes enums structs resources
  >> checkEnums enums
  >> checkStructs structs enums
  >> checkResources resources structs
  >> return spec

-- | Looks for name clashes in the definitions.
checkClashes :: [EnumType] -> [StructType] -> [Resource] -> Err ()
checkClashes enums structs resources =
  let duplicates = findDuplicatesInDefinitions enums structs in
    case duplicates of
       Nothing -> return ()
       Just name ->
           fail $ "Name clash in declaration: "
               ++ name
               ++ " is declared more than once."

-- | Check all enums, making sure there is not repeated values.
checkEnums :: [EnumType] -> Err ()
checkEnums enums = F.forM_ enums checkEnumValues
  where
    checkEnumValues :: EnumType -> Err ()
    checkEnumValues (DefEnum (Ident name) vals) =
      case duplicates of
        Nothing -> return ()
        Just dup -> fail $ "Enum value defined more than once: " ++ dup
      where
        duplicates = findDuplicates $ map (\(EnVal (Ident n)) -> n) vals

-- | Makes sure all the types used in the struct definitions are valid.
checkStructs :: [StructType] -> [EnumType] -> Err ()
checkStructs structs enums =
  F.forM_ structs structOk
    where
      structOk :: StructType -> Err ()
      structOk (DefStr (Ident name) fields) = F.forM_ fields fieldOk
      fieldOk :: Field -> Err ()
      fieldOk (FDefined _ (Ident name) _) =
        unless (name `elem` getPossibleFieldTypes enums structs)
               (fail $ "The type (" ++ name ++ ") was not defined.")
      fieldOk _ = return ()

-- | TODO
checkResources :: [Resource] -> [StructType] -> Err ()
checkResources _ _ = return ()

getEnumNames :: [EnumType] -> [String]
getEnumNames = map getEnumName
  where
    getEnumName (DefEnum (Ident name) _) = name

getStructNames :: [StructType] -> [String]
getStructNames = map getStructName
  where
    getStructName (DefStr (Ident name) _) = name

getResourceNames :: [Resource] -> [String]
getResourceNames = map getResourceName
  where
    getResourceName (DefResNoOp (Ident name) _ _) = name

getPossibleFieldTypes :: [EnumType] -> [StructType] -> [String]
getPossibleFieldTypes enums structs = getEnumNames enums ++ getStructNames structs

findDuplicatesInDefinitions :: [EnumType] -> [StructType] -> Maybe String
findDuplicatesInDefinitions enums structs =
  findDuplicates $ enumNames ++ structNames
  where
    enumNames = getEnumNames enums
    structNames = getStructNames structs

findDuplicates :: [String] -> Maybe String
findDuplicates list = go list S.empty S.empty
  where
    go [] _ repeatedSet = Nothing
    go (x:xs) seen repeatedSet | lowerCaseStr x `S.member` seen = return x
    go (x:xs) seen repeatedSet = go xs (lowerCaseStr x `S.insert` seen) repeatedSet
    lowerCaseStr :: String -> String
    lowerCaseStr = map toLower

