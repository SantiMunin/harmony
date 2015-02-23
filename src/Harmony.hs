module Main where

import           Control.Monad.Error
import           Control.Monad.State
import qualified Data.Set            as Set
import qualified Data.Text.Lazy      as TL
import qualified Data.Text.Lazy.IO   as TL
import           Language.Abs
import           Language.ErrM
import           Language.Lex
import           Language.Par
import           Language.Print
import           ServiceGenerator
import           StaticCheck
import           System.Environment  (getArgs)
import qualified TemplateCompiler    as TC


main :: IO ()
main = do
  args <- getArgs
  case args of
       [file] -> do
         contents <- readFile file
         case pSpecification (myLexer contents) of
           Bad err -> error $ show err
           Ok tree -> case staticCheck tree of
                        Bad err -> error $ show err
                        Ok _ -> generateOutput tree
       _ -> error "Usage: harmony <source_file>"
  return ()

generateOutput :: Specification -> IO ()
generateOutput spec =
  do
    putStrLn "Rendering backend"
    -- TODO: generalize so different templates can be used
    output <- TC.render "templates/node_js.tpl" $ generateService spec
    TL.putStrLn output
