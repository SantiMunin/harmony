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
    Hidden
  | Immutable
  | Required
  | PrimaryKey deriving (Eq, Ord, Show)

-- | A field has a type, an identifier and a list of modifiers.
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

type Enums = M.Map Id EnumInfo

type Structs = M.Map Id StructInfo

type Resources = M.Map Id (Route, Writable)

type Writable = Bool

-- | The spec of an api is a set of enums and structs, along with the resources.
data ApiSpec = AS { name      :: String
                  , version   :: String
                  , enums     :: Enums
                  , structs   :: Structs
                  , resources :: Resources }

derive makeArbitrary ''Type
derive makeArbitrary ''Modifier

instance (Arbitrary FieldInfo) where
  arbitrary = do
    n <- arbitrary
    t <- arbitrary
    mods <- listOf arbitrary
    return $ FI (n, t, S.fromList mods)

-- | Gets the primary key of a struct if it was specified.
getPrimaryKey :: StructInfo -> Maybe Id
getPrimaryKey structInfo =
  case filter hasPkModifier structInfo of
    [] -> Nothing
    [FI (x, _, _)] -> Just x
    _ -> error "A struct should have at most one specified primary key."
  where
    hasPkModifier (FI (_, _, modifiers)) = PrimaryKey `S.member` modifiers
