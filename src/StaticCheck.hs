-- | Static checking of the input file.
module StaticCheck (staticCheck) where

import qualified ApiSpec             as AS
import           Control.Arrow
import           Control.Monad
import           Control.Monad.State as CMS
import           Data.Char
import qualified Data.DiGraph        as DG
import qualified Data.Foldable       as F
import qualified Data.Map            as M
import           Data.Maybe
import qualified Data.Set            as S
import           Language.Abs
import           Language.ErrM
import           LangUtils


type Env = ( S.Set String -- ^ All the struct names
           , AS.ApiSpec -- ^ API info
           )

type StaticCheck a = StateT Env Err a

-- | List of reserved words.
reservedWords :: [String]
-- TODO(18): complete list
reservedWords = ["Long", "Int", "String", "Resource", "Enum", "Struct"]

-- | Initial environment (all empty).
initialEnv :: Env
initialEnv = (S.empty,
              AS.AS { AS.name = ""
                    , AS.version = ""
                    , AS.requiresAuth = False
                    , AS.enums = M.empty
                    , AS.structs = []
                    , AS.resources = M.empty
                    }
             )

-- | Checks the definition for:
--   * Name clashes
--   * Undefined types
-- and returns a better representation of the api spec.
staticCheck :: Specification -> Err AS.ApiSpec
staticCheck spec@(Spec _ _ modules enums structs resources) = do
  (_, (_, apiSpec)) <- runStateT checkSeq initialEnv
  return apiSpec
  where
    checkSeq = do
      CMS.modify (\(strs, s) -> (strs, s { AS.name = specName spec, AS.version = specVersion spec }))
      let enumNames = getEnumNames enums
          structNames = getStructNames structs
          customTypeNames = enumNames ++ structNames
      checkClashes $ customTypeNames ++ reservedWords

      CMS.modify (\(_, as) -> (S.fromList structNames, as))
      readAndCheckEnums enums
      addStructNames structs
      processModules modules
      readAndCheckStructs structs
      checkResources resources
      sortStructsByDependencyOrder

-- | Looks for name clashes in the definitions.
-- Note that it skips resources as those need to have the same name as some struct.
checkClashes :: [String] -> StaticCheck ()
checkClashes names =
  case findDuplicates names of
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
      CMS.modify (\(structNames, as) ->
                     (structNames, as { AS.enums = M.insert name (map enumValName vals) (AS.enums as)}))

-- | Adds struct names (without information). Useful to check if a field is valid even if the type is a struct defined afterwards.
addStructNames :: [StructType] -> StaticCheck ()
addStructNames strs = F.forM_ strs addStr
  where
    addStr (DefStr (Ident strName) _) = CMS.modify $ Control.Arrow.first (S.insert strName)

-- | Modifies the environment with the given modules
processModules :: Modules -> StaticCheck ()
processModules EmptyMods = return ()
processModules (Mods moduleDefs) = void (F.forM_ moduleDefs processModule)
  where
    processModule :: Ident -> StaticCheck ()
    processModule (Ident m) | map toLower m == "authentication"  = CMS.modify (\(structNames, as) -> (structNames, as { AS.requiresAuth = True }))
                            | otherwise = fail $ "Error processing module: " ++ show m ++ " is not supported."

-- | Makes sure all the types used in the struct definitions are valid.
readAndCheckStructs :: [StructType] -> StaticCheck ()
readAndCheckStructs strs = F.forM_ strs structOk >> F.forM_ strs readStruct
    where
      structOk :: StructType -> StaticCheck ()
      structOk (DefStr (Ident strName) fields) = do
        names <- getAllNamesFromEnv
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
      fieldOk (FDef _ _ (FDefined (Ident name))) = do
        knownTypes <- getAllNamesFromEnv
        unless (name `S.member` knownTypes)
               (fail $ "The type (" ++ name ++ ") was not defined.")
      fieldOk _ = return ()

      checkHasPk :: String -> [Field] -> StaticCheck ()
      checkHasPk strName fields =
        unless (length (filter ((==AS.PrimaryKey) . readAnnotation) $ concatMap fieldAnnotations fields) <= 1)
               (fail $ strName ++ " Primary Key annotation (@PK) is used more than once")

      readStruct :: StructType -> StaticCheck ()
      readStruct (DefStr (Ident name) fields) = do
        enums <- getEnumNamesFromEnv
        CMS.modify (\(strs, as) ->
                     (strs, as { AS.structs = (name, map (readField (strs, enums)) fields):AS.structs as}))
      readAnnotation (Ann (Ident name)) | map toLower name == "hidden" = AS.Hidden
                                        | map toLower name == "pk" = AS.PrimaryKey
                                        | map toLower name == "unique" = AS.Unique
                                        | map toLower name == "immutable" = AS.Immutable
                                        | map toLower name == "required" = AS.Required
                                        | otherwise = error $ "Annotation " ++ name ++ " not recognized."
      readField envInfo (FDef anns (Ident n) ft) = AS.FI (n, fieldSpecType envInfo ft, S.fromList $ map readAnnotation anns)

-- | Gets enum names from the environment.
getEnumNamesFromEnv :: StaticCheck (S.Set String)
getEnumNamesFromEnv = do
  as <- CMS.gets snd
  return $ S.fromList (map fst $ M.toList $ AS.enums as)

-- | Gets the names (identifiers) of all structs and enums (form the environment).
getAllNamesFromEnv :: StaticCheck (S.Set String)
getAllNamesFromEnv = do
  (strNames, as) <- CMS.get
  return $ strNames `S.union` S.fromList (map fst $ M.toList $ AS.enums as)

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
      unless (isJust $ lookup (resName res) definedStructs)
             (fail $ "Resource " ++ resName res ++ " does not refer to a defined struct.")
    addResource res = CMS.modify (\(names, as) -> (names, as { AS.resources = M.insert (resName res) (resRoute res, resIsWritable res) (AS.resources as)}))

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

-- | Builds a dependency graph to sort the structs so forall a,b -> if "a" depends on "b", "b" is
-- first in the list. It fails if there is a cycle in the graph.
sortStructsByDependencyOrder :: StaticCheck ()
sortStructsByDependencyOrder = do
  apiSpec <- CMS.gets snd
  let g = createGraph apiSpec
  unless (not $ DG.hasCycle g) (fail "There is a cycle in the struct definitions")
  let priorityList = DG.generateInverseDependencyList g
  let sortedStructs = matchOrder priorityList (AS.structs apiSpec)
  CMS.modify (\(ns, as) -> (ns, as { AS.structs = sortedStructs }))
    where
      createGraph :: AS.ApiSpec -> DG.Graph AS.Id
      createGraph apiSpec = foldl (\g str -> DG.addNeighbors (fst str) (getDeps str) g) DG.empty (AS.structs apiSpec)
        where
          getDeps (_, fields) = map (\(AS.FI (_, t, _)) -> AS.strName t) $ filter AS.isStructField fields
      matchOrder :: Eq a => [a] -> [(a, b)] -> [(a,b)]
      matchOrder pList list = go pList list []
        where
          go :: Eq a => [a] -> [(a, b)] -> [(a, b)] -> [(a, b)]
          go [] _ acc = reverse acc
          go (x:xs) list acc = go xs list ((x, fromJust (lookup x list)):acc)
