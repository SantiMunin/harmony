#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription (HookedBuildInfo, emptyHookedBuildInfo)
import System.Process (system)
import System.Exit (ExitCode (..))

main = defaultMainWithHooks $ simpleUserHooks { preBuild = makeBnfc }

-- Execute bnfc to compile the language specification (see language_spec/Language.cf) before 
-- compiling.  
makeBnfc :: Args -> BuildFlags -> IO HookedBuildInfo
makeBnfc _ _ = do 
  bnfcOutput <- system "bnfc -d language_spec/Language.cf"
  case bnfcOutput of
       ExitSuccess -> putStrLn "Bnfc file successfully generated"
       (ExitFailure code) -> error "Error compiling the language specification: " ++ show code
  mvOutput <- system "rm -rf src/Language; mv Language src"
  case mvOutput of
       (ExitFailure code) -> error "Error moving the generated files to src/Language"
       _ -> putStrLn "Generated files moved to src/Language"
  return emptyHookedBuildInfo
