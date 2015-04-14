{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Defines an api spec (to be built after the static checking of the AST) and helper methods over
-- it.
module ApiSpec where

import           Control.Monad   (liftM)
import           Data.DeriveTH
import qualified Data.Map        as M
import qualified Data.Set        as S
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
  deriving (Eq, Ord, Show)

derive makeArbitrary ''Modifier

-- | A field has a type, an identifier and a set of modifiers.
newtype FieldInfo = FI (Id, Type, S.Set Modifier) deriving (Show, Eq, Ord)

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
          | TList Type deriving (Eq, Ord, Show)

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
data ApiSpec = AS { name      :: String -- ^ Name of the service
                  , version   :: String -- ^ Version of the service
                  , enums     :: Enums -- ^ Information about the user defined enums
                  , structs   :: Structs -- ^ Information about the user defined structs
                  , resources :: Resources -- ^ Information about the resources defined
                  }

derive makeShow ''ApiSpec

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
nonEmptyString = listOf1 arbitrary

-- | Generates an arbitrary 'FieldInfo', making sure that the ids used for enums and structs are valid.
generateRandomFieldInfo :: [Id] -> [Id] -> Gen FieldInfo
generateRandomFieldInfo enumIds structIds =
  do
    id <- nonEmptyString
    t <- generateRandomType enumIds structIds
    modifiers <- listOf arbitrary
    return $ FI (id, t, S.fromList modifiers)
  where
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
                        else liftM TEnum $ elements enumIds
        processStruct =  if null structIds
                          then generateRandomType enumIds structIds
                          else liftM TStruct $ elements structIds
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
    version' <- nonEmptyString
    enumIds <- listOf nonEmptyString
    enums' <- mapM createEnum enumIds
    structIds <- listOf nonEmptyString
    structs' <- mapM (createStruct enumIds structIds) structIds
    resources' <- mapM (createResource . fst) structs'
    return AS { name = name'
              , version = version'
              , enums = M.fromList enums'
              , structs = structs'
              , resources = M.fromList resources'
              }
    where
      createEnum :: Id -> Gen (Id, EnumInfo)
      createEnum id = do
        values <- listOf1 nonEmptyString
        return (id, values)

      createStruct :: [Id] -> [Id] -> Id -> Gen (Id, StructInfo)
      createStruct enumIds structIds thisStructId = do
        rawFields <- listOf1 $ generateRandomFieldInfo enumIds structIds
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

instance (Arbitrary Type) where
  arbitrary = do
    t <- elements [TInt, TLong, TFloat, TDouble, TBool, TString, TEnum "", TStruct "", TList TInt]
    case t of
         TEnum _ -> liftM TEnum nonEmptyString
         TStruct _ -> liftM TStruct nonEmptyString
         TList _ -> liftM TList arbitrary
         other -> return other
