{-# LANGUAGE OverloadedStrings #-}
module Generation.OutputGenerator(TemplateInfo, createGenInfo, GenerationInfo, generateOutput) where

import qualified ApiSpec                     as AS
import           Control.Monad
import           Data.List
import qualified Data.Text.Lazy.IO           as TL
import qualified Generation.ServiceGenerator as SG
import qualified Generation.TemplateCompiler as TC
import           Paths_Harmony
import           System.Directory

data GenerationInfo = GI { files :: [FilePath], templates :: [TemplateInfo] , fieldMapping :: AS.Type -> String }
type TemplateInfo = (FilePath, String)

createGenInfo :: [FilePath] -> [TemplateInfo] -> (AS.Type -> String) -> GenerationInfo
createGenInfo files' templates' fieldMapping' =
  GI { files = files', templates = templates', fieldMapping = fieldMapping' }

generateOutput :: FilePath -> AS.ApiSpec -> GenerationInfo -> IO ()
generateOutput outputPath apiSpec genInfo = do
  forM_ (files genInfo) (copy outputPath)
  forM_ (templates genInfo) (generateAndCopy outputPath $ SG.generateService apiSpec (fieldMapping genInfo))

copy :: FilePath -> FilePath -> IO ()
copy origin dest = do
  cabalFilePath <- getDataFileName origin
  copyFile cabalFilePath (dest ++ "/" ++ origin)

generateAndCopy :: FilePath -> TC.Service -> TemplateInfo -> IO ()
generateAndCopy dest service (templatePath, newExt)  = do
  output <- TC.render templatePath service
  createDirectoryIfMissing {- create parent dirs too -} True destDir
  TL.writeFile destFile output
  where
    destFileWithoutExt = dest ++ takeWhile (/= '.') (dropWhile (/= '/') templatePath)
    destDir =
      let indices = elemIndices '/' destFile
      in if null indices then "." else take (last indices + 1) destFile
    destFile = destFileWithoutExt ++ "." ++ newExt
