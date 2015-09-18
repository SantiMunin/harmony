{-# LANGUAGE OverloadedStrings #-}
-- | Contains the generation logic and functions of the different possible outputs.
module Generation.OutputGenerator(
    GenerationFunction
  , generateJSServer
  , generateJSClient
  , generatePythonClient
  , generateJavaClient )
  where

import           Control.Monad
import           Data.List
import qualified Data.Text.Lazy.IO           as TL
import qualified Generation.ServiceGenerator as SG
import qualified Generation.TemplateCompiler as TC
import           Paths_harmony
import           System.Directory
import           System.Exit                 (ExitCode (..))
import           System.Log.Formatter        ()
import           System.Log.Handler          ()
import           System.Log.Handler.Simple   ()
import           System.Log.Handler.Syslog   ()
import           System.Log.Logger
import           System.Process              (system)
import qualified TypeCheck.ApiSpec           as AS

-- | A template is a source file and a new extension; e.g. ("template.tpl", "js") will result in
--   "template.js" after the template compilation.
type TemplateInfo = (FilePath, String)

-- | All the info necesaary in order to generate a target:
--   * List of regular files (this will be just directly copied)
--   * List of templates that will be compiled
--   * A map from a Harmony type to the target's type
--   * A map from a Harmony type to the target's type (boxed, for Java)
type GenerationInfo = ([FilePath], [TemplateInfo], AS.Type -> String, AS.Type -> String)

-- | A function that generates the target.
type GenerationFunction = FilePath -- ^ Output path
                       -> AS.ApiSpec -- ^ Information of the defined web service
                       -> IO ()

-- | Target generation function.
generateJSServer, generateJSClient, generatePythonClient, generateJavaClient :: GenerationFunction
generateJSServer = generateOutput (files, templates, fieldMapping, fieldMappingBoxedType) postOpFunc
  where
    files = []
    templates = [ ("templates/server/js/server.tpl", "js")
                , ("templates/server/js/package.tpl", "json")
                ]
    fieldMapping AS.TBool = "Boolean"
    fieldMapping AS.TString = "String"
    fieldMapping AS.TLong = "Number"
    fieldMapping AS.TInt = "Number"
    fieldMapping AS.TDouble = "SchemaTypes.Double"
    -- TODO: try to use Float instead of Double
    fieldMapping AS.TFloat = "SchemaTypes.Double"
    -- An enum is an string with constraints.
    fieldMapping (AS.TEnum _) = "String"
    fieldMapping (AS.TStruct t) = t
    fieldMapping (AS.TList t) = fieldMapping t
    fieldMapping other = error $ "Javascript server generation: Type not recognized -> " ++ show other
    fieldMappingBoxedType _ = error "generateJSServer: Javascript has no boxed types"

generateJSClient = error "Javascript client is not implemented yet"

generatePythonClient = generateOutput (files, templates, fieldMapping, fieldMappingBoxedType) postOpFunc
  where
    files = [ "templates/client/python/requirements.txt" ]
    templates = [ ("templates/client/python/client.tpl", "py")
                , ("templates/client/python/test.tpl", "py")
                ]
    -- These are the generators for the different types used by Hypothesis.
    fieldMapping AS.TBool = "booleans()"
    fieldMapping AS.TString = "non_empty_string_generator"
    fieldMapping AS.TInt = "integers(-1000,1000)"
    fieldMapping AS.TLong = "integers(-1000,1000)"
    -- TODO: check handling of floats in Python
    fieldMapping AS.TDouble = "floats(-1000.0, 1000.0)"
    fieldMapping AS.TFloat = "floats(-1000.0, 1000.0)"
    fieldMapping (AS.TEnum _) = "error: no directly translation from enum type to Hypothesis type"
    fieldMapping (AS.TStruct name) = name ++ "Data"
    fieldMapping (AS.TList t) = "[" ++ fieldMapping t ++ "]"
    fieldMapping other = error $ "Python client generation: Type not recognized -> " ++ show other
    fieldMappingBoxedType _ = error "generatePythonClient: Python has no boxed types"

generateJavaClient = generateOutput (files, templates, fieldMapping, fieldMappingBoxedType) postOpFunc
  where
    files = [ "templates/client/java/pom.xml"
            , "templates/client/java/src/main/java/com/prototype/NetworkClient.java"
            ]
    templates = [ ("templates/client/java/src/main/java/com/prototype/ServiceClient.tpl", "java") ]
    fieldMapping AS.TBool = "boolean"
    fieldMapping AS.TString = "String"
    fieldMapping AS.TInt = "int"
    fieldMapping AS.TLong = "long"
    fieldMapping AS.TDouble = "double"
    fieldMapping AS.TFloat = "float"
    fieldMapping (AS.TEnum enumName) = enumName
    fieldMapping (AS.TStruct strName) = strName
    fieldMapping (AS.TList t) = fieldMapping t
    fieldMapping other = error $ "Java client generation: Type not recognized -> " ++ show other
    fieldMappingBoxedType AS.TBool = "Boolean"
    fieldMappingBoxedType AS.TString = "String"
    fieldMappingBoxedType AS.TInt = "Integer"
    fieldMappingBoxedType AS.TLong = "Long"
    fieldMappingBoxedType AS.TDouble = "Double"
    fieldMappingBoxedType AS.TFloat = "Float"
    fieldMappingBoxedType (AS.TEnum enumName) = enumName
    fieldMappingBoxedType (AS.TStruct strName) = strName
    fieldMappingBoxedType (AS.TList t) = "List<" ++ fieldMappingBoxedType t ++ ">"
    fieldMappingBoxedType other = error $ "Java client generation: Boxed type not recognized -> " ++ show other

-- | Applies different post processing operations to each file type.
postOpFunc :: String -> FilePath -> IO ()
postOpFunc "js" = applyJsBeautify
postOpFunc "py" = applyYapf
postOpFunc "java" = removeUselessCommasAndApplyAStyle
postOpFunc _ = \_ -> return ()

-- | Applies a beautifier (beautify-js) to generated Javascript code. It does nothing if the tool is not available.
applyJsBeautify :: FilePath -> IO ()
applyJsBeautify path = do
  infoM "Generation.OutputGenerator" $ "Applying js-beautifier to " ++ path
  outcome <- system $ "js-beautify " ++ path ++ " > tempfile && cat tempfile > " ++ path ++ " && rm tempfile"
  case outcome of
    ExitSuccess -> return ()
    (ExitFailure _) ->
      warningM "Generation.OutputGenerator" $ "There was a problem applying the Javascript beautifier, "
              ++ "please check if it is installed and in the system's path (if "
              ++ "you ignore this message the Python generated files will not be properly formatted)."

-- | Applies a beatufier (yapf) to generated Python code. It does nothing if the tool is not available in the path.
applyYapf :: FilePath -> IO ()
applyYapf path = do
  infoM "Generation.OutputGenerator" $ "Applying yapf to " ++ path
  outcome <- system $ "yapf " ++ path ++ " > tempfile && cat tempfile > " ++ path ++ " && rm tempfile"
  case outcome of
    ExitSuccess -> return ()
    (ExitFailure _) ->
      warningM "Generation.OutputGenerator" $ "There was a problem applying the Python beautifier, "
              ++ "please check if it is installed and in the system's path (if "
              ++ "you ignore this message the Python generated files will not be properly formatted)."

-- | Removes useless commas that break the compilation (e.g., public MyClass(int a, int b, )) and then applies
-- a beautifier (AStyle). It does not apply the beautifier if the tool in not available in the path.
removeUselessCommasAndApplyAStyle :: FilePath -> IO ()
removeUselessCommasAndApplyAStyle path = do
  infoM "Generation.OutputGenerator" $ "Postprocessing " ++ path
  removeCommas <- system $ "sed 's/,[^,]*) {.*$/\\) {/' "
                  ++ path ++ " > tempFile && cat tempFile > "
                  ++ path ++ " && rm tempFile"
  case removeCommas of
    ExitSuccess -> return ()
    (ExitFailure _) ->
      error $ "Unable to apply Java postprocessing, please"
              ++ " open an issue in www.github.com/SantiMunin/harmony/issues"
  removeCommas2 <- system $ "sed 's/, *\\().*\\)$/\\1/' "
                  ++ path ++ " > tempFile && cat tempFile > "
                  ++ path ++ " && rm tempFile"
  case removeCommas2 of
    ExitSuccess -> return ()
    (ExitFailure _) ->
      error $ "Unable to apply Java postprocessing, please"
              ++ " open an issue in www.github.com/SantiMunin/harmony/issues"
  infoM "Generation.OutputGenerator" $ "Applying astyle to " ++ path
  outcome <- system $ "astyle " ++ path
  case outcome of
    ExitSuccess -> do
      system $ "rm " ++ path ++ ".orig"
      return ()
    (ExitFailure _) ->
      warningM "Generation.OutputGenerator" $ "There was a problem applying the Java beautifier, "
              ++ "please check if it is installed and in the system's path (if "
              ++ "you ignore this message the Java generated files will not be properly formatted)."


-- | Uses all the information provided by the user (and the input file) and generates
-- the output by compiling the templates and copying all the files to the output directory.
generateOutput :: GenerationInfo -- ^ The information gathered from the user
               -> (String -> FilePath -> IO ()) -- ^ A function that performs a different operation per file extension
               -> FilePath -- ^ Output path
               -> AS.ApiSpec -- ^ The information gathered from the input file (specification)
               -> IO ()
generateOutput (files, templates, fieldMapping, fieldMappingBoxedType) postOpFunc outputPath apiSpec = do
  updateGlobalLogger "Generation.OutputGenerator" (setLevel INFO)
  forM_ files (`copy` outputPath)
  forM_ templates (generateAndWrite outputPath (SG.generateService apiSpec fieldMapping fieldMappingBoxedType) postOpFunc)

-- | Copies a file.
copy :: FilePath -- ^ Origin file
     -> FilePath -- ^ Destination path
     -> IO ()
copy origin dest = do
  let destFile = dest ++ dropWhile (/= '/')  origin
  let destDir = dirName destFile
  infoM "Generation.OutputGenerator" $ "Copying " ++ show destFile
  cabalFilePath <- getDataFileName origin
  createDirectoryIfMissing {- create parent dirs too -} True destDir
  copyFile cabalFilePath destFile

-- | Gets the dir of a file path.
dirName :: FilePath -> FilePath
dirName file = let indices = elemIndices '/' file
               in if null indices then "." else take (last indices + 1) file

-- | Generate a template and writes it to the destination path.
generateAndWrite :: FilePath -- ^ Destination path
                -> TC.Service -- ^ Information for the template
                -> (String -> FilePath -> IO ()) -- ^ A function that performs a different operation per file extension
                -> TemplateInfo -- ^ Information of the template
                -> IO ()
generateAndWrite dest service postOpFunc (templatePath, newExt) = do
  infoM "Generation.OutputGenerator" $ "Creating " ++ show destFile
  output <- TC.render templatePath service
  createDirectoryIfMissing {- create parent dirs too -} True destDir
  TL.writeFile destFile output
  postOpFunc newExt destFile
  where
    destFileWithoutExt = dest ++ takeWhile (/= '.') (dropWhile (/= '/') templatePath)
    destDir = dirName destFile
    destFile = destFileWithoutExt ++ "." ++ newExt
