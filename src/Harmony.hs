module Main where

import           Control.Monad.Error
import           Control.Monad.State
import qualified Data.Set                   as Set
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.IO          as TL
import qualified Generation.OutputGenerator as OG
import           Language.Abs
import           Language.ErrM
import           Language.Lex
import           Language.Par
import           Language.Print
import           StaticCheck
import           System.Environment         (getArgs)

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
  -- TODO: use flags to collect information for client and server, tests should be always the same
  OG.generateOutput
      "/tmp/harmony_output"
     (OG.createGenInfo [] [("templates/js/server/server.tpl", "js"), ("templates/js/server/package.tpl", "json")])
     spec
