{-# LANGUAGE TemplateHaskell #-}

module TestUtils (assertException) where

import           Control.Exception
import           Control.Monad
import           Data.DeriveTH
import           Language.Abs
import           Test.HUnit
import           Test.QuickCheck

-- | Asserts that some computation throws a concrete error.
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
  handleJust isWanted (const $ return ()) $ do
  action
  assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)

-- TODO(#42)
derive makeArbitrary ''Ident
derive makeArbitrary ''Name
derive makeArbitrary ''Version
derive makeArbitrary ''VerIdent
derive makeArbitrary ''Specification
derive makeArbitrary ''EnumType
derive makeArbitrary ''EnumVal
derive makeArbitrary ''StructType
derive makeArbitrary ''Field
derive makeArbitrary ''Annotation
derive makeArbitrary ''FType
derive makeArbitrary ''Resource
derive makeArbitrary ''Mode
