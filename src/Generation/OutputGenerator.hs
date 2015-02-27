{-# LANGUAGE OverloadedStrings #-}
module Generation.OutputGenerator(TemplateInfo, createGenInfo, generateOutput) where

import           Control.Monad
import           Data.List
import qualified Data.Text.Lazy.IO           as TL
import qualified Generation.ServiceGenerator as SG
import           Generation.TemplateCompiler
import           Language.Abs
import           Paths_Harmony
import           System.Directory

data GenerationInfo = GI { files :: [FilePath], templates :: [TemplateInfo] }
type TemplateInfo = (FilePath, String)

createGenInfo :: [FilePath] -> [TemplateInfo] -> GenerationInfo
createGenInfo files' templates' = GI { files = files', templates = templates' }

generateOutput :: FilePath -> GenerationInfo -> Specification -> IO ()
generateOutput outputPath genInfo spec = do
  forM_ (files genInfo) (copy outputPath)
  forM_ (templates genInfo) (generateAndCopy outputPath $ generateServiceInfo spec)

copy :: FilePath -> FilePath -> IO ()
copy origin dest = do
  cabalFilePath <- getDataFileName origin
  copyFile cabalFilePath (dest ++ "/" ++ origin)

generateAndCopy :: FilePath -> Service -> TemplateInfo -> IO ()
generateAndCopy dest service (templatePath, newExt)  = do
  output <- render templatePath service
  createDirectoryIfMissing {- create parent dirs too -} True destDir
  TL.writeFile destFile output
  where
    destFileWithoutExt = takeWhile (/= '.') $ dest ++ dropWhile (/= '/') templatePath
    destDir = take (last (elemIndices '/' destFile) + 1) destFile
    destFile = destFileWithoutExt ++ "." ++ newExt

-- TODO(9): add type mapping information
generateServiceInfo :: Specification -> Service
generateServiceInfo = SG.generateService

