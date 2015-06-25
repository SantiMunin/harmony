{-# LANGUAGE OverloadedStrings #-}
-- | Contains the generation logic and functions of the different possible outputs.
module Generation.OutputGenerator(GenerationFunction, generateJSServer, generateJSClient, generatePythonClient) where

import qualified ApiSpec                     as AS
import           Control.Monad
import           Data.List
import qualified Data.Text.Lazy.IO           as TL
import qualified Generation.ServiceGenerator as SG
import qualified Generation.TemplateCompiler as TC
import           Paths_harmony
import           System.Directory
import           System.Exit                 (ExitCode (..))
import           System.Log.Formatter
import           System.Log.Handler          (setFormatter)
import           System.Log.Handler.Simple
import           System.Log.Handler.Syslog
import           System.Log.Logger
import           System.Process              (system)

-- | A template is a source file and a new extension; e.g. ("template.tpl", "js") will result in
--   "template.js" after the template compilation.
type TemplateInfo = (FilePath, String)

-- | All the info necesaary in order to generate a target:
--   * List of regular files (this will be just directly copied)
--   * List of templates that will be compiled
--   * A map from a Harmony type to the target's type
type GenerationInfo = ([FilePath], [TemplateInfo], AS.Type -> String)

-- | A function that generates the target.
type GenerationFunction = FilePath -- ^ Output path
                       -> AS.ApiSpec -- ^ Information of the defined web service
                       -> IO ()

-- | Target generation function.
generateJSServer, generateJSClient, generatePythonClient :: GenerationFunction
generateJSServer = generateOutput (files, templates, fieldMapping) postOpFunc
  where
    files = []
    templates = [ ("templates/server/js/server.tpl", "js")
                , ("templates/server/js/package.tpl", "json")
                ]
    fieldMapping AS.TString = "String"
    fieldMapping AS.TLong = "Number"
    fieldMapping AS.TInt = "Number"
    fieldMapping AS.TDouble = "Number"
    -- An enum is an string with constraints.
    fieldMapping (AS.TEnum _) = "String"
    fieldMapping (AS.TStruct t) = t
    fieldMapping (AS.TList t) = fieldMapping t
    fieldMapping other = error $ "Javascript server generation: Type not recognized -> " ++ show other

generateJSClient = error "Javascript client is not implemented yet"

generatePythonClient = generateOutput (files, templates, fieldMapping) postOpFunc
  where
    files = []
    templates = [ ("templates/client/python/client.tpl", "py")
                , ("templates/client/python/test.tpl", "py")
                ]
    -- These are the generators for the different types used by Hypothesis.
    fieldMapping AS.TString = "strategy([strategy(integers_in_range(65,90)) | strategy(integers_in_range(97, 122))]).map(lambda l: map(chr, l)).map(lambda l: ''.join(l))"
    fieldMapping AS.TInt = "integers_in_range(-1000,1000)"
    fieldMapping AS.TLong = "integers_in_range(-1000,1000)"
    fieldMapping AS.TDouble = "error:PythonNoTypes (Double)"
    fieldMapping (AS.TEnum _) = "error: no directly translation from enum type to Hypothesis type"
    fieldMapping (AS.TStruct name) = name ++ "Data"
    fieldMapping (AS.TList t) = "[" ++ fieldMapping t ++ "]"
    fieldMapping other = error $ "Python client generation: Type not recognized -> " ++ show other

-- | Applies different post processing operations to each file type.
postOpFunc :: String -> FilePath -> IO ()
postOpFunc "js" = applyJsBeautify
postOpFunc "py" = applyYapf
postOpFunc _ = \_ -> return ()

applyJsBeautify :: FilePath -> IO ()
applyJsBeautify path = do
  infoM "Generation.OutputGenerator" $ "Applying js-beautifier to " ++ path
  outcome <- system $ "js-beautify " ++ path ++ " > tempfile && cat tempfile > " ++ path ++ " && rm tempfile"
  case outcome of
    ExitSuccess -> return ()
    (ExitFailure _) ->
      warningM "Generation.OutputGenerator" $ "There was a problem applying the python beautifier, "
              ++ "please check it is installed and in the system's path (if "
              ++ "you ignore this message the Python generated files will not be properly formatted"


applyYapf :: FilePath -> IO ()
applyYapf path = do
  infoM "Generation.OutputGenerator" $ "Applying yapf to " ++ path
  outcome <- system $ "yapf " ++ path ++ " > tempfile && cat tempfile > " ++ path ++ " && rm tempfile"
  case outcome of
    ExitSuccess -> return ()
    (ExitFailure _) ->
      warningM "Generation.OutputGenerator" $ "There was a problem applying the python beautifier, "
              ++ "please check it is installed and in the system's path (if "
              ++ "you ignore this message the Python generated files will not be properly formatted"

-- | Uses all the information provided by the user (and the input file) and generates
-- the output by compiling the templates and copying all the files to the output directory.
generateOutput :: GenerationInfo -- ^ The information gathered from the user
               -> (String -> FilePath -> IO ()) -- ^ A function that performs a different operation per file extension
               -> FilePath -- ^ Output path
               -> AS.ApiSpec -- ^ The information gathered from the input file (specification)
               -> IO ()
generateOutput (files, templates, fieldMapping) postOpFunc outputPath apiSpec = do
  updateGlobalLogger "Generation.OutputGenerator" (setLevel INFO)
  forM_ files (copy outputPath)
  forM_ templates (generateAndWrite outputPath (SG.generateService apiSpec fieldMapping) postOpFunc)

-- | Copies a file.
copy :: FilePath -- ^ Origin file
     -> FilePath -- ^ Destination path
     -> IO ()
copy origin dest = do
  cabalFilePath <- getDataFileName origin
  copyFile cabalFilePath (dest ++ "/" ++ origin)

-- | Generate a template and writes it to the destination path.
generateAndWrite :: FilePath -- ^ Destination path
                -> TC.Service -- ^ Information for the template
                -> (String -> FilePath -> IO ()) -- ^ A function that performs a different operation per file extension
                -> TemplateInfo -- ^ Information of the template
                -> IO ()
generateAndWrite dest service postOpFunc (templatePath, newExt) = do
  output <- TC.render templatePath service
  createDirectoryIfMissing {- create parent dirs too -} True destDir
  TL.writeFile destFile output
  postOpFunc newExt destFile
  where
    destFileWithoutExt = dest ++ takeWhile (/= '.') (dropWhile (/= '/') templatePath)
    destDir =
      let indices = elemIndices '/' destFile
      in if null indices then "." else take (last indices + 1) destFile
    destFile = destFileWithoutExt ++ "." ++ newExt
