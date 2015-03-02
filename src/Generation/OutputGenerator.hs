{-# LANGUAGE OverloadedStrings #-}
module Generation.OutputGenerator(TemplateInfo, createGenInfo, GenerationInfo, generateOutput) where

import           Control.Monad
import           Data.List
import qualified Data.Text.Lazy.IO           as TL
import qualified Generation.ServiceGenerator as SG
import qualified Generation.TemplateCompiler as TC
import           Language.Abs
import           Paths_Harmony
import           System.Directory

data GenerationInfo = GI { files :: [FilePath], templates :: [TemplateInfo] , fieldMapping :: Field -> String }
type TemplateInfo = (FilePath, String)

createGenInfo :: [FilePath] -> [TemplateInfo] -> (Field -> String) -> GenerationInfo
createGenInfo files' templates' fieldMapping' =
  GI { files = files', templates = templates', fieldMapping = fieldMapping' }

generateOutput :: FilePath -> Specification -> GenerationInfo -> IO ()
generateOutput outputPath spec genInfo = do
  forM_ (files genInfo) (copy outputPath)
  forM_ (templates genInfo) (generateAndCopy outputPath $ SG.generateService spec (fieldMapping genInfo))

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
    destFileWithoutExt = takeWhile (/= '.') $ dest ++ dropWhile (/= '/') templatePath
    destDir = take (last (elemIndices '/' destFile) + 1) destFile
    destFile = destFileWithoutExt ++ "." ++ newExt
