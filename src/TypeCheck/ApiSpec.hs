{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Defines an api spec (to be built after the static checking of the AST) and helper methods over
-- it.
module TypeCheck.ApiSpec where

import           Control.Applicative ((<$>))
import           Data.DeriveTH
import           Data.List
import qualified Data.Map            as M
import qualified Data.Set            as S
import           Test.QuickCheck

-- | Identifier of an enum, struct, field...
type Id = String

-- | The route of a resource.
type Route = String

-- | An enum value.
type EnumValue = String

-- | An enum is a list of values.
type EnumInfo = [EnumValue]

-- | A field modifier.
data Modifier =
    Hidden -- ^ The field will not be returned when read
  | Immutable -- ^ The field cannot be modified
  | Required -- ^ The field can't be absent
  | PrimaryKey -- ^ The field is the primary key (thus 'Unique' as well)
  | Unique -- ^ The field can't have repeated values throughout the collection
  | UserLogin -- ^ The field value will contain the user's login
  deriving (Eq, Ord, Show)

derive makeArbitrary ''Modifier

-- | A field has a type, an identifier and a set of modifiers.
newtype FieldInfo = FI (Id, Type, S.Set Modifier) deriving (Eq, Ord)

instance (Show FieldInfo) where
  show (FI (name, type', modifiers)) = intercalate "\n" [printModifiers modifiers, "  " ++ name ++ ": " ++ show type']
    where
      printModifiers modifiers = intercalate "\n  " $ map (\m -> "@" ++ show m) (S.toList modifiers)

-- | A struct is a list of fields.
type StructInfo = [FieldInfo]

-- | A resource has a route and a write mode.
type ResourceInfo = (Route, Writable)

-- | A type can be a primitive one (int, long, double, bool...), an enum, a struct, or a list of
-- another type.
data Type = TInt
          | TLong
          | TFloat
          | TDouble
          | TBool
          | TString
          | TEnum Id
          | TStruct Id
          | TList Type deriving (Eq, Ord)

instance (Show Type) where
  show TInt = "Int"
  show TLong = "Long"
  show TFloat = "Float"
  show TDouble = "Double"
  show TBool = "Boolean"
  show TString = "String"
  show (TEnum name) = name
  show (TStruct name) = name
  show (TList type') = "[" ++ show type' ++ "]"

instance (CoArbitrary Type) where
  coarbitrary = coarbitraryShow

-- | Map from enum id to its info.
type Enums = M.Map Id EnumInfo

-- | Map from struct id to its info.
type Structs = [(Id, StructInfo)]

-- | Map from resource id to the route and the mode.
type Resources = M.Map Id ResourceInfo

-- | Writable is a boolean type.
type Writable = Bool

-- | The spec of an api is a set of enums and structs, along with the resources.
data ApiSpec = AS { name         :: String -- ^ Name of the service
                  , version      :: String -- ^ Version of the service
                  , requiresAuth :: Bool -- ^ Whether it should support authentication or not
                  , enums        :: Enums -- ^ Information about the user defined enums
                  , structs      :: Structs -- ^ Information about the user defined structs
                  , resources    :: Resources -- ^ Information about the resources defined
                  }

instance (Show ApiSpec) where
  show as = intercalate "\n" [ printName as
                             , printVersion as
                             , "\n" ++ printModules as
                             , "\n" ++ printEnums as
                             , "\n" ++ printStructs as
                             , "\n" ++ printResources as
                             ]
    where
      printName as = "service_name: " ++ name as
      printVersion as = "service_version: " ++ version as
      printModules as | requiresAuth as = "require modules [ Authentication ]"
                      | otherwise = ""
      printEnums as = intercalate "\n" $ map printEnum (M.toList $ enums as)
        where
          printEnum (name, info) = "enum " ++ name ++ " { " ++ printEnumValues info ++ " }"
          printEnumValues = intercalate ", "
      printStructs as = intercalate "\n\n" $ map printStruct (structs as)
        where
          printStruct (name, info) = "struct " ++ name ++ " {\n" ++ intercalate ",\n" (printFields info) ++ "\n}"
          printFields = map (\f -> "  " ++ show f)
      printResources as = intercalate "\n" $ map printResource (M.toList $ resources as)
        where
          printResource (structName, (route, writable)) = "resource " ++ structName ++ " (\"/" ++ route ++ "\")" ++ if writable then "" else " read_only"

-- | Gets the primary key of a struct if it was specified.
getPrimaryKey :: StructInfo -- ^ The info of the struct
              -> Maybe Id -- ^ The result (Nothing if there was no PK defined)
getPrimaryKey structInfo =
  case filter hasPkModifier structInfo of
    [] -> Nothing
    [FI (x, _, _)] -> Just x
    _ -> error "A struct should have at most one specified primary key."
  where
    hasPkModifier (FI (_, _, modifiers)) = PrimaryKey `S.member` modifiers

-- | Finds out if a fieldinfo is relative to a struct
isStructField :: FieldInfo -> Bool
isStructField (FI (_, t, _)) = isStruct t

-- | Find outs if a type is a struct
isStruct :: Type -> Bool
isStruct (TStruct _) = True
isStruct (TList t) = isStruct t
isStruct _ = False

-- | Get the name of a struct
strName :: Type -> String
strName (TStruct t) = t
strName (TList t) = strName t
strName other = error $ "strName of a non struct type (" ++ show other ++ ")"

-- Testing

-- | Generates a non-empty arbitrary 'String'.
nonEmptyString :: Gen String
nonEmptyString = listOf1 (elements ['a'..'z'])

-- | Generates an arbitrary 'FieldInfo', making sure that the ids used for enums and structs are valid.
generateRandomFieldInfo :: Bool -> [Id] -> [Id] -> Gen FieldInfo
generateRandomFieldInfo withAuthentication enumIds structIds =
  do
    id <- nonEmptyString
    t <- generateRandomType enumIds structIds
    nModifiers <- oneof $ map return [0..2]
    modifiers <- vectorOf nModifiers arbitrary
    let modifiers' = if withAuthentication then modifiers else removeAuthentication modifiers
    return $ FI (id, t, S.fromList modifiers')
  where
    removeAuthentication :: [Modifier] -> [Modifier]
    removeAuthentication modifiers = go modifiers []
      where
        go [] acc = acc
        go (x:xs) acc = if x == UserLogin then go xs acc else go xs (x:acc)
    generateRandomType :: [Id] -> [Id] -> Gen Type
    generateRandomType enumIds structIds = do
      t <- arbitrary

      case t of
           (TStruct _) -> processStruct
           (TEnum _) -> processEnum
           (TList t') | needsEnumId t' ->
                          if null enumIds
                            then generateRandomType enumIds structIds
                            else do
                                newId <- elements enumIds
                                return $ TList $ setNewId newId t'
                      | needsStructId t' ->
                          if null structIds
                            then generateRandomType enumIds structIds
                            else do
                              newId <- elements structIds
                              return $ TList $ setNewId newId t'
                      | otherwise -> return $ TList t'
           other -> return other
      where
        processEnum = if null enumIds
                        then generateRandomType enumIds structIds
                        else TEnum <$> elements enumIds
        processStruct =  if null structIds
                          then generateRandomType enumIds structIds
                          else TStruct <$> elements structIds
        needsEnumId (TEnum _) = True
        needsEnumId (TList t) = needsEnumId t
        needsEnumId _ = False

        needsStructId (TStruct _) = True
        needsStructId (TList t) = needsStructId t
        needsStructId _ = False

        setNewId newId' (TEnum _) = TEnum newId'
        setNewId newId' (TStruct _) = TStruct newId'
        setNewId newId' (TList t) = TList $ setNewId newId' t
        setNewId _ t = t

instance (Arbitrary ApiSpec) where
  arbitrary = do
    name' <- nonEmptyString
    -- TODO: make arbitrary
    let version' = "1.0.0"
    withAuthentication <- arbitrary
    nEnums <- oneof $ map return [0..5]
    enumIds <- generateStringsWithoutClashes nEnums S.empty
    enums' <- mapM createEnum enumIds
    nStructs <- oneof $ map return [1..7]
    structIds <- generateStringsWithoutClashes nStructs (S.fromList enumIds)
    structs' <- mapM (createStruct enumIds structIds withAuthentication) structIds
    resources' <- mapM (createResource . fst) structs'
    return AS { name = name'
              , version = version'
              , requiresAuth = withAuthentication
              , enums = M.fromList enums'
              , structs = structs'
              , resources = M.fromList resources'
              }
    where
      createEnum :: Id -> Gen (Id, EnumInfo)
      createEnum id = do
        values <- listOf1 nonEmptyString
        return (id, values)

      createStruct :: [Id] -> [Id] -> Bool -> Id -> Gen (Id, StructInfo)
      createStruct enumIds structIds withAuthentication thisStructId = do
        nFields <- oneof $ map return [1..8]
        rawFields <- vectorOf nFields $ generateRandomFieldInfo withAuthentication enumIds structIds
        shouldHavePk <- arbitrary
        let fields = filterPrimaryKey shouldHavePk rawFields
        return (thisStructId, fields)
        where
          filterPrimaryKey _ [] = []
          filterPrimaryKey True (FI (id, t, mods):fs) | PrimaryKey `S.member` mods = FI (id, t, PrimaryKey `S.delete` mods) : filterPrimaryKey False fs
          filterPrimaryKey True (FI (id, t, mods):fs) = FI (id, t, mods) : filterPrimaryKey True fs
          filterPrimaryKey False (FI (id, t, mods):fs) = FI (id, t, PrimaryKey `S.delete` mods) : filterPrimaryKey False fs

      createResource :: Id -> Gen (Id, ResourceInfo)
      createResource id = do
        route <- nonEmptyString
        writable <- arbitrary
        return (id, (route, writable))

      generateStringsWithoutClashes :: Int -> S.Set String -> Gen [String]
      generateStringsWithoutClashes n strs = go n strs []
        where
          go :: Int -> S.Set String -> [String] -> Gen [String]
          go 0 _ acc = return acc
          go n strs acc = do
            new <- nonEmptyString
            if S.member new strs
            then go n strs acc
            else go (n-1) (S.insert new strs) (new:acc)


instance (Arbitrary Type) where
  arbitrary = do
    t <- elements [TInt, TLong, TFloat, TDouble, TBool, TString, TEnum "", TStruct "", TList TInt]
    case t of
         TEnum _ -> TEnum <$> nonEmptyString
         TStruct _ -> TStruct <$> nonEmptyString
         TList _ -> TList <$> arbitrarySingleType
         other -> return other
      where
        -- TODO: add TEnum and TList
        arbitrarySingleType = elements [TInt, TLong, TFloat, TDouble, TBool, TString]
