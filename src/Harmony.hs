module Main where

import           Control.Monad              (forM_, unless)
import           Data.Maybe                 (fromJust)
import qualified Generation.OutputGenerator as OG
import           Language.Abs
import           Language.ErrM
import           Language.Par
import qualified StaticCheck                as SC
import           System.Console.GetOpt
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (ExitCode (ExitFailure), exitWith)

-- | Flags of the executable
data Flag = Client String
          | Server String
          | OutputDir String
          deriving (Eq, Show)

-- | Definition of the flags expected by the executable
options :: [OptDescr Flag]
options = [ Option ['c'] ["client"]
                   (OptArg (Client . fromJust) "CLIENT") "Desired output for the client"
          , Option ['s'] ["server"]
                   (OptArg (Server . fromJust) "SERVER") "Desired output for the server"
          ]

-- | Entry point, parses arguments, then the specification file and generates all the desired
-- | outputs.
main :: IO ()
main = do
  (inputFile, outputPath, toGenerateList) <- parseArgs
  contents <- readFile inputFile
  case pSpecification (myLexer contents) of
    Bad err -> ioError (userError err)
    Ok tree -> case SC.staticCheck tree of
                 Bad err -> ioError (userError err)
                 Ok _ -> generateOutput tree toGenerateList outputPath
  return ()

-- | Parses the arguments and returns a list of outputs to generate.
parseArgs :: IO (FilePath, FilePath, [OG.GenerationInfo])
parseArgs = do
  args <- getArgs
  programName <- getProgName

  case getOpt Permute options args of
    (flags, paths, []) -> do
      unless (length paths == 2)
             (printUsageAndExitWithError Nothing programName options)
      unless (not $ null flags)
             (printUsageAndExitWithError (Just "Select at least a client/server") programName options)
      return (head paths, paths !! 1, map getGenInfo $ filter isLanguageOutput flags)
        where
          isLanguageOutput (OutputDir _) = False
          isLanguageOutput _ = True
    -- TODO: check if this should be another printUsageAndExitWithError call
    (_, _, errs) -> ioError (userError (concat errs ++ usage programName options))


-- | Generate all the output required.
generateOutput :: Specification -> [OG.GenerationInfo] -> FilePath -> IO ()
generateOutput spec genInfos outputPath = forM_ genInfos (OG.generateOutput outputPath spec)

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
    header = "Usage: " ++ progName ++ " [OPTION...] input_file output_path"

-- | Prints usage and exits with error.
printUsageAndExitWithError :: Maybe String -> String -> [OptDescr Flag] -> IO ()
printUsageAndExitWithError message progName options = do
  case message of
    Just str -> putStrLn $ "ERROR: " ++ str ++ "\n"
    Nothing -> return ()
  putStrLn $ usage progName options
  exitWith (ExitFailure 2)

