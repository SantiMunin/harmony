{-# LANGUAGE TemplateHaskell #-}

module TestUtils where

import           Data.DeriveTH
import           Language.Abs
import           Test.QuickCheck

-- TODO(#42)
derive makeArbitrary ''Ident
derive makeArbitrary ''RouteIdent
derive makeArbitrary ''Name
derive makeArbitrary ''Version
derive makeArbitrary ''VerIdent
derive makeArbitrary ''Modules
derive makeArbitrary ''Specification
derive makeArbitrary ''EnumType
derive makeArbitrary ''EnumVal
derive makeArbitrary ''StructType
derive makeArbitrary ''Field
derive makeArbitrary ''Annotation
derive makeArbitrary ''FType
derive makeArbitrary ''Resource
derive makeArbitrary ''Mode
