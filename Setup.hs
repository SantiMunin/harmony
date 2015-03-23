#!/usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.PackageDescription (HookedBuildInfo, emptyHookedBuildInfo)
import System.Process (system)
import System.Exit (ExitCode (..))
import System.Environment (getArgs)

main = defaultMainWithHooks $ simpleUserHooks { preBuild = makeBnfc }

-- Execute bnfc to compile the language specification (see language-spec/Language.cf) before 
-- compiling.  
makeBnfc :: Args -> BuildFlags -> IO HookedBuildInfo
makeBnfc _ _ = do 
  bnfcOutput <- system "./make_bnfc.sh"
  case bnfcOutput of
       ExitSuccess -> return emptyHookedBuildInfo
       (ExitFailure code) -> error $ "Error processing the language specification: " ++ show code
