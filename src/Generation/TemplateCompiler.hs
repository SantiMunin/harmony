{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Compiles Hastache templates and defines the datatypes to pass information to them.
module Generation.TemplateCompiler where

import           Data.Data
import           Data.DeriveTH
import qualified Data.Text.Lazy        as TL
import           Data.Typeable         ()
import           Paths_harmony
import           Test.QuickCheck
import           Text.Hastache
import           Text.Hastache.Context

-- | An enum value
data StrValue = StrValue { value :: String } deriving (Show, Data, Typeable, Eq)
data EnumValues = EnumValue { values :: [StrValue] } deriving (Show, Data, Typeable, Eq)

-- | A schema variable. It is a field of a struct.
-- <b>DISCLAIMER</b>: there is some redundancy but the focus of this module is to make the templating easy.
data SchemaVar = SchemaVar { varName     :: String
                           , varType     :: String
                           , isList      :: Bool
                           -- Only if it is an enum (varType == String and values not empty)
                           , isEnum      :: Maybe EnumValues
                           , isStruct    :: Bool
                           , isKey       :: Bool
                           , isRequired  :: Bool
                           , isHidden    :: Bool
                           , isUnique    :: Bool
                           , isUserLogin :: Bool
                           } deriving (Show, Data, Typeable, Eq)

-- | A schema is a struct (it has a name, a route, a write mode, etc...).
data Schema = Schema { schemaName  :: String
                     , schemaRoute :: Maybe StrValue
                     , writable    :: Bool
                     , hasKeyField :: Bool
                     , keyField    :: String
                     , schemaVars  :: [SchemaVar] } deriving (Show, Data, Typeable)

-- | A service has a name, a version, and a list of 'Schema'
data Service = Service { name         :: String
                       , version      :: String
                       , requiresAuth :: Bool
                       , schema       :: [Schema] } deriving (Show, Data, Typeable)

-- | Given a template and a service object, render the template.
render:: String -> Service -> IO TL.Text
render templateLoc service =
  do
    template <- getDataFileName templateLoc >>= readFile
    let context = mkGenericContext service in
      hastacheStr defaultConfig (encodeStr template) context


derive makeArbitrary ''Service
derive makeArbitrary ''StrValue
derive makeArbitrary ''Schema
derive makeArbitrary ''SchemaVar
derive makeArbitrary ''EnumValues

