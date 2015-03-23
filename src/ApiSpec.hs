{-# LANGUAGE TemplateHaskell #-}
-- | Defines an api spec (to be built after the static checking of the AST) and helper methods over
-- it.
module ApiSpec where

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
  | PrimaryKey -- ^ The field is the primary key (at most one per struct)
  deriving (Eq, Ord, Show)

-- | A field has a type, an identifier and a set of modifiers.
newtype FieldInfo = FI (Id, Type, S.Set Modifier) deriving Show

-- | A struct is a list of fields.
type StructInfo = [FieldInfo]

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
          | TList Type deriving (Eq, Show)

-- | Map from enum id to its info.
type Enums = M.Map Id EnumInfo

-- | Map from struct id to its info.
type Structs = M.Map Id StructInfo

-- | Map from resource id to the route and the mode.
type Resources = M.Map Id (Route, Writable)

-- | Writable is a boolean type.
type Writable = Bool

-- | The spec of an api is a set of enums and structs, along with the resources.
data ApiSpec = AS { name      :: String -- ^ Name of the service
                  , version   :: String -- ^ Version of the service
                  , enums     :: Enums -- ^ Information about the user defined enums
                  , structs   :: Structs -- ^ Information about the user defined structs
                  , resources :: Resources -- ^ Information about the resources defined
                  }


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


derive makeArbitrary ''Type
derive makeArbitrary ''Modifier

instance (Arbitrary FieldInfo) where
  arbitrary = do
    n <- arbitrary
    t <- arbitrary
    mods <- listOf arbitrary
    return $ FI (n, t, S.fromList mods)
