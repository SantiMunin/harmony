module StaticCheck (staticCheck) where

import           Control.Arrow
import           Control.Monad       (unless)
import           Data.Char
import qualified Data.Foldable       as F
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S
import           Language.Abs
import           Language.ErrM
import           LangUtils

import           Control.Monad.State as CMS

data StateEnv = SEnv { enums     :: M.Map String [String]
                     , structs   :: M.Map String [(String, String)]
                     , resources :: M.Map String String
                     }

type StaticCheck a = StateT StateEnv Err a

validAnnotations :: [String]
validAnnotations = ["PK", "Hidden", "Fixed"]

initialEnv :: StateEnv
initialEnv = SEnv { enums = M.empty, structs = M.empty, resources = M.empty }

-- | Checks the definition for:
--   * Name clashes
--   * Undefined types
staticCheck :: Specification -> Err ()
staticCheck spec@(Spec _ _ enums structs resources) = evalStateT checkSeq initialEnv
  where
    checkSeq = readAndCheckEnums enums >> readAndCheckStructs structs >> checkResources resources

-- | Looks for name clashes in the definitions.
--  Note that it skips resources are those need to have the same name as some struct.
checkClashes :: [EnumType] -> [StructType] -> StaticCheck ()
checkClashes enums structs =
  let duplicates = findDuplicatesInDefinitions enums structs in
    case duplicates of
       Nothing -> return ()
       Just name ->
           fail $ "Name clash in declaration: "
               ++ name
               ++ " is declared more than once."

-- | Check all enums, making sure there are not repeated values.
readAndCheckEnums :: [EnumType] -> StaticCheck ()
readAndCheckEnums es = F.forM_ es checkEnumValues
  where
    checkEnumValues :: EnumType -> StaticCheck ()
    checkEnumValues (DefEnum (Ident name) vals) = do
      let duplicates = findDuplicates (map enumValName vals) in
        unless (isNothing duplicates) (fail $ "Enum value defined more than once " ++ fromJust duplicates)
      CMS.modify (\s -> s { enums = M.insert name (map enumValName vals) (enums s)})

-- | Makes sure all the types used in the struct definitions are valid.
readAndCheckStructs :: [StructType] -> StaticCheck ()
readAndCheckStructs strs = F.forM_ strs structOk >> F.forM_ strs readStruct
    where
      structOk :: StructType -> StaticCheck ()
      structOk (DefStr (Ident name) fields) = F.forM_ fields fieldOk

      fieldOk :: Field -> StaticCheck()
      fieldOk (FDefined _ (Ident name) _) = do
        definedEnums <- CMS.gets enums
        unless (name `M.member` definedEnums)
               (fail $ "The type (" ++ name ++ ") was not defined.")
      fieldOk _ = return ()

      readStruct :: StructType -> StaticCheck ()
      readStruct (DefStr (Ident name) fields) =
        CMS.modify (\s -> s { structs = M.insert name (map (fieldName &&& fieldType) fields) (structs s) })

-- | TODO
checkResources :: [Resource] -> StaticCheck ()
checkResources ress = do
  F.forM_ ress resourceOk
  F.forM_ ress addResource
  where
    resourceOk res = do
      definedStructs <- CMS.gets structs
      unless (resName res `M.member` definedStructs)
             (fail $ "Resource " ++ resName res ++ " does not refer to a defined struct.")
    addResource res = CMS.modify (\s -> s { resources = M.insert (resRoute res) (resName res) (resources s)} )

getEnumNames :: [EnumType] -> [String]
getEnumNames = map enumName

getStructNames :: [StructType] -> [String]
getStructNames = map strName

getResourceNames :: [Resource] -> [String]
getResourceNames = map resName

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

