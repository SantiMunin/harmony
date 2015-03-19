module StaticCheck (staticCheck) where

import qualified ApiSpec             as AS
import           Control.Monad.State as CMS
import           Data.Char
import qualified Data.Foldable       as F
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S
import           Language.Abs
import           Language.ErrM
import           LangUtils

type Env = (S.Set String, AS.ApiSpec)

type StaticCheck a = StateT Env Err a

-- | List of reserved words.
reservedWords :: [String]
-- TODO(18): complete list
reservedWords = ["Int", "String", "Resource", "Enum", "Struct"]

-- | Initial environment (all empty).
initialEnv :: Env
initialEnv = (S.empty,
              AS.AS { AS.name = ""
                    , AS.version = ""
                    , AS.enums = M.empty
                    , AS.structs = M.empty
                    , AS.resources = M.empty
                    }
             )

-- | Checks the definition for:
--   * Name clashes
--   * Undefined types
staticCheck :: Specification -> Err AS.ApiSpec
staticCheck spec@(Spec _ _ enums structs resources) = do
  (_, s) <- runStateT checkSeq initialEnv
  return $ snd s
  where
    checkSeq = do
      CMS.modify (\(n, s) -> (n, s { AS.name = specName spec, AS.version = specVersion spec }))
      let customTypeNames = getEnumNames enums ++ getStructNames structs
      checkClashes $ customTypeNames ++ reservedWords

      CMS.modify (\(_, as) -> (S.fromList customTypeNames, as))
      readAndCheckEnums enums
      readAndCheckStructs structs
      checkResources resources

-- | Looks for name clashes in the definitions.
-- Note that it skips resources as those need to have the same name as some struct.
checkClashes :: [String] -> StaticCheck ()
checkClashes names =
  let duplicates = findDuplicates names in
    case duplicates of
       Nothing -> return ()
       Just name ->
           fail $ "Name clash in declaration: "
               ++ name
               ++ " is declared more than once (or it is a reserved word)."

-- | Reads all enums, making sure there are not repeated values.
readAndCheckEnums :: [EnumType] -> StaticCheck ()
readAndCheckEnums es = F.forM_ es checkEnumValues >> F.forM_ es readEnum
  where
    checkEnumValues :: EnumType -> StaticCheck ()
    checkEnumValues (DefEnum _ vals) =
      let duplicates = findDuplicates (map enumValName vals) in
        unless (isNothing duplicates) (fail $ "Enum value defined more than once " ++ fromJust duplicates)
    readEnum :: EnumType -> StaticCheck ()
    readEnum (DefEnum (Ident name) vals) =
      CMS.modify (\(names, as) ->
                     (names, as { AS.enums = M.insert name (map enumValName vals) (AS.enums as)}))

-- | Makes sure all the types used in the struct definitions are valid.
readAndCheckStructs :: [StructType] -> StaticCheck ()
readAndCheckStructs strs = F.forM_ strs structOk >> F.forM_ strs readStruct
    where
      structOk :: StructType -> StaticCheck ()
      structOk (DefStr (Ident strName) fields) = do
        names <- CMS.gets fst
        checkAttributeClashes fields (names `S.union` S.fromList reservedWords)
        F.forM_ fields fieldOk
        checkHasPk strName fields

      checkAttributeClashes :: [Field] -> S.Set String -> StaticCheck (S.Set String)
      checkAttributeClashes fields alreadySeen =
        F.foldlM (\names name ->
          if name `S.member` names
          then fail $ "Field " ++ name
                   ++ " is defined twice (or it is a reserved word/custom type name)"
          else return $ name `S.insert` names) alreadySeen (map fieldName fields)

      fieldOk :: Field -> StaticCheck ()
      fieldOk (FDefined _ (Ident name) _) = do
        knownTypes <- CMS.gets fst
        unless (name `S.member` knownTypes)
               (fail $ "The type (" ++ name ++ ") was not defined.")
      fieldOk _ = return ()

      checkHasPk :: String -> [Field] -> StaticCheck ()
      checkHasPk strName fields =
        unless ((length $ filter ((==AS.PrimaryKey) . readAnnotation) $ concatMap fieldAnnotations fields) <= 1)
               (fail $ strName ++ " Primary Key annotation (@PK) is used more than once")

      readStruct :: StructType -> StaticCheck ()
      readStruct (DefStr (Ident name) fields) =
        CMS.modify (\(names, as) ->
                     (names, as { AS.structs = M.insert name (map (readField as) fields) (AS.structs as)}))
      readAnnotation (Ann (Ident name)) | map toLower name == "hidden" = AS.Hidden
                                        | map toLower name == "pk" = AS.PrimaryKey
                                        | map toLower name == "immutable" = AS.Immutable
                                        | map toLower name == "required" = AS.Required
                                        | otherwise = error $ "Annotation " ++ name ++ " not recognized."
      readField as (FDefined anns (Ident n) (Ident t)) = (n, getType as t, map readAnnotation anns)
        where
          getType env t | t `M.member` AS.enums env = AS.TEnum t
                        | t `M.member` AS.structs env = AS.TStruct t
                        | otherwise = error $ "getType: " ++ t ++ " is not defined."

      readField _ (FString anns (Ident n)) = (n, AS.TString, map readAnnotation anns)
      readField _ (FInt anns (Ident n)) = (n, AS.TInt, map readAnnotation anns)
      readField _ (FDouble anns (Ident n)) = (n, AS.TDouble, map readAnnotation anns)

checkResources :: [Resource] -> StaticCheck ()
checkResources ress = do
  checkDifferentRoutes ress
  F.forM_ ress resourceOk
  F.forM_ ress addResource
  where
    checkDifferentRoutes ress =
      case findDuplicates (map resRoute ress) of
           Nothing -> case findDuplicates (map resName ress) of
                        Nothing -> return ()
                        (Just repeated') -> fail $ "Struct " ++ repeated' ++ " is referred more than once by a resource."
           (Just repeated) -> fail $ "Route " ++ repeated ++ " is defined more than once"
    resourceOk res = do
      definedStructs <- CMS.gets (\(_, as) -> AS.structs as)
      unless (resName res `M.member` definedStructs)
             (fail $ "Resource " ++ resName res ++ " does not refer to a defined struct.")
    addResource res = CMS.modify (\(names, as) -> (names, as { AS.resources = M.insert (resName res) (resRoute res) (AS.resources as)}))

getEnumNames :: [EnumType] -> [String]
getEnumNames = map enumName

getStructNames :: [StructType] -> [String]
getStructNames = map strName

findDuplicates :: [String] -> Maybe String
findDuplicates list = go list S.empty S.empty
  where
    go [] _ _ = Nothing
    go (x:_) seen _ | lowerCaseStr x `S.member` seen = return x
    go (x:xs) seen repeatedSet = go xs (lowerCaseStr x `S.insert` seen) repeatedSet
    lowerCaseStr :: String -> String
    lowerCaseStr = map toLower

