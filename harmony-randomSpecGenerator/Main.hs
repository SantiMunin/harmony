module Main where

import           Test.QuickCheck   (Gen, arbitrary, generate)
import           TypeCheck.ApiSpec

main :: IO ()
main = do
  apiSpec <- generate (arbitrary :: Gen ApiSpec)
  print apiSpec
