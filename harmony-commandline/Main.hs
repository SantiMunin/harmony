module Main where

import           Control.Monad              (forM_, unless)
import           Data.Maybe                 (fromJust)
import qualified Generation.OutputGenerator as OG
import           Language.ErrM
import           Language.Par
import           System.Console.GetOpt
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (ExitCode (ExitFailure), exitWith)
import qualified TypeCheck.ApiSpec          as AS
import qualified TypeCheck.StaticCheck      as SC

-- | Flags of the executable
data Options = Options
  { generationFunctions :: [OG.GenerationFunction]
  , outputDir           :: FilePath
  }

defaultOptions :: Options
defaultOptions = Options
  { generationFunctions = []
  , outputDir = "./harmony_output"
  }

-- | Definition of the flags expected by the executable
options :: [OptDescr (Options -> Options)]
options = [ Option "c" ["client"]
                   (OptArg (\c options -> options { generationFunctions = parseClient c:generationFunctions options})
                           "CLIENTS")
                   "Desired output for the client: {-cpython}"
          , Option "s" ["server"]
                   (OptArg (\s options -> options { generationFunctions = parseServer s:generationFunctions options})
                           "SERVERS")
                   "Desired output for the server: {-sjs}"
          , Option "o" ["output_dir"]
                   (OptArg (\dir options -> options { outputDir = fromJust dir })
                           "OUTPUT_DIR")
                   "Output path"
          ]

-- | Parse flag input
parseClient, parseServer :: Maybe String -> OG.GenerationFunction
parseClient (Just "js") = OG.generateJSClient
parseClient (Just "python") = OG.generatePythonClient
parseClient (Just other) = error $ "Could not parse client: " ++ other
parseClient Nothing = error "parseClient: not expected Nothing as flag"
parseServer (Just "js") = OG.generateJSServer
parseServer (Just other) = error $ "Could not parse server" ++ other
parseServer Nothing = error "parseServer: not expected Nothing as flag"

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
                 Ok env -> generateOutput env toGenerateList outputPath
  return ()

-- | Parses the arguments and returns a list of targets to generate.
parseArgs :: IO (FilePath, FilePath, [OG.GenerationFunction])
parseArgs = do
  args <- getArgs
  programName <- getProgName

  case getOpt Permute options args of
    (flags, args, []) -> do
      unless (length args == 1)
             (printUsageAndExitWithError Nothing programName options)
      unless (not $ null flags)
             (printUsageAndExitWithError (Just "Select at least a client/server") programName options)
      return (head args, outputDir info, generationFunctions info)
        where
          info :: Options
          info = foldl (flip ($))  defaultOptions flags
    -- TODO: check if this should be another printUsageAndExitWithError call
    (_, _, errs) -> ioError (userError (concat errs ++ usage programName options))

-- | Generate all the output required.
generateOutput :: AS.ApiSpec -> [OG.GenerationFunction] -> FilePath -> IO ()
generateOutput apiSpec genFunctions outputPath = forM_ genFunctions (\f -> f outputPath apiSpec)

-- | Generates the usage message.
usage :: String -> [OptDescr (Options -> Options)] -> String
usage progName = usageInfo header
  where
    header = "Usage: " ++ progName ++ " [OPTION...] input_file"

-- | Prints usage and exits with error.
printUsageAndExitWithError :: Maybe String -> String -> [OptDescr (Options -> Options)] -> IO ()
printUsageAndExitWithError message progName options = do
  case message of
    Just str -> putStrLn $ "ERROR: " ++ str ++ "\n"
    Nothing -> return ()
  putStrLn $ usage progName options
  exitWith (ExitFailure 2)

