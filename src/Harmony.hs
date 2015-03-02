module Main where

import           Control.Monad              (forM_, unless)
import           Data.Maybe                 (fromMaybe)
import qualified Generation.OutputGenerator as OG
import           Language.Abs
import           Language.ErrM
import           Language.Par
import qualified StaticCheck                as SC
import           System.Console.GetOpt
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (exitSuccess)

-- | Flags of the executable
data Flag = Client String | Server String deriving (Eq, Show)

-- | Definition of the flags expected by the executable
options :: [OptDescr Flag]
options = [ Option ['c'] ["client"]
                   (OptArg clientp "CLIENT") "Desired output for the client (default: none)"
          , Option ['s'] ["server"]
                   (OptArg serverp "SERVER") "Desired output for the server (default: node.js)"
          ]
  where
    clientp, serverp :: Maybe String -> Flag
    clientp = Client . fromMaybe ""
    serverp = Server . fromMaybe "js"

-- | Entry point, parses arguments, then the specification file and generates all the desired
-- | outputs.
main :: IO ()
main = do
  (inputFile, toGenerateList) <- parseArgs
  contents <- readFile inputFile
  case pSpecification (myLexer contents) of
    Bad err -> ioError (userError err)
    Ok tree -> case SC.staticCheck tree of
                 Bad err -> ioError (userError err)
                 Ok _ -> generateOutput tree toGenerateList
  return ()

-- | Parses the arguments and returns a list of outputs to generate.
parseArgs :: IO (FilePath, [OG.GenerationInfo])
parseArgs = do
  args <- getArgs
  programName <- getProgName
  unless(length args >= 2) (printUsageAndExitWithError programName options)

  case getOpt Permute options args of
    (flags, inputFile, []) -> do
      unless (length inputFile == 1) (printUsageAndExitWithError programName options)
      unless (not $ null flags) (ioError (userError "Select at least a client/server"))
      return (head inputFile, map getGenInfo flags)
    (_, _, errs) -> ioError (userError (concat errs ++ usage programName options))

-- | Generate all the output required.
generateOutput :: Specification -> [OG.GenerationInfo] -> IO ()
generateOutput spec genInfos = forM_ genInfos (OG.generateOutput "/tmp/harmony_output" spec)

-- TODO: think about moving this to a different module to centralize the specification of all the
-- implemented outputs.
-- | Maps flags to generation information.
getGenInfo :: Flag -> OG.GenerationInfo
getGenInfo (Server "js") = OG.createGenInfo files templates fieldMapping
  where
    files = []
    templates = [ ("templates/server/js/server.tpl", "js")
                , ("templates/server/js/package.tpl", "json") ]
    fieldMapping (FString _ _) = "String"
    fieldMapping (FInt _ _) = "Number"
    fieldMapping (FDouble _ _) = "Number"
    fieldMapping _ = error "Custom types not implemented yet"
getGenInfo other = error $ "Couldn't process " ++ show other ++ " flag."

-- | Generates the usage message.
usage :: String -> [OptDescr Flag] -> String
usage progName = usageInfo header
  where
    header = "Usage: " ++ progName ++ " [OPTION...] input_file"

-- | Prints usage and exits with error.
printUsageAndExitWithError :: String -> [OptDescr Flag] -> IO ()
printUsageAndExitWithError progName options = putStrLn progName options >> exitFailure 1

