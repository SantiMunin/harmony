module Main where

import qualified ApiSpec                    as AS
import           Control.Monad              (forM_, unless)
import           Data.Maybe                 (fromJust)
import qualified Generation.OutputGenerator as OG
import           Language.ErrM
import           Language.Par
import qualified OutputSelection            as OS
import qualified StaticCheck                as SC
import           System.Console.GetOpt
import           System.Environment         (getArgs, getProgName)
import           System.Exit                (ExitCode (ExitFailure), exitWith)

-- | Flags of the executable
data Options = Options
  { targets   :: [OS.Target]
  , outputDir :: FilePath
  } deriving Show

defaultOptions :: Options
defaultOptions = Options
  { targets = []
  , outputDir = "./harmony_output"
  }

-- | Definition of the flags expected by the executable
options :: [OptDescr (Options -> Options)]
options = [ Option ['c'] ["client"]
                   (OptArg (\c options -> options { targets = parseClient c:targets options})
                           "CLIENTS")
                   "Desired output for the client"
          , Option ['s'] ["server"]
                   (OptArg (\s options -> options { targets = parseServer s:targets options})
                           "SERVERS")
                   "Desired output for the server"
          , Option ['o'] ["output_dir"]
                   (OptArg (\dir options -> options { outputDir = fromJust dir })
                           "OUTPUT_DIR")
                   "Output path"
          ]

-- | Parse flag input
parseClient, parseServer :: Maybe String -> OS.Target
parseClient (Just "js") = OS.CJavascript
parseClient (Just "python") = OS.CPython
parseClient (Just other) = error $ "Could not parse client: " ++ other
parseClient Nothing = error "parseClient: not expected Nothing as flag"
parseServer (Just "js") = OS.SJavascript
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

-- | Parses the arguments and returns a list of outputs to generate.
parseArgs :: IO (FilePath, FilePath, [OG.GenerationInfo])
parseArgs = do
  args <- getArgs
  programName <- getProgName

  case getOpt Permute options args of
    (flags, args, []) -> do
      unless (length args == 1)
             (printUsageAndExitWithError Nothing programName options)
      unless (not $ null flags)
             (printUsageAndExitWithError (Just "Select at least a client/server") programName options)
      return (head args, outputDir info, desiredOutputs)
        where
          info :: Options
          info = foldl (flip ($))  defaultOptions flags
          desiredOutputs = map OS.getGenInfo $ targets info
    -- TODO: check if this should be another printUsageAndExitWithError call
    (_, _, errs) -> ioError (userError (concat errs ++ usage programName options))

-- | Generate all the output required.
generateOutput :: AS.ApiSpec -> [OG.GenerationInfo] -> FilePath -> IO ()
generateOutput apiSpec genInfos outputPath = forM_ genInfos (OG.generateOutput outputPath apiSpec)

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
