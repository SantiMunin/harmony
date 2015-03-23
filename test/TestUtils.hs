module TestUtils (assertException) where

import           Control.Exception
import           Control.Monad
import           Test.HUnit

-- | Asserts that some computation throws a concrete error.
assertException :: (Exception e, Eq e) => e -> IO a -> IO ()
assertException ex action =
  handleJust isWanted (const $ return ()) $ do
  action
  assertFailure $ "Expected exception: " ++ show ex
  where isWanted = guard . (== ex)
