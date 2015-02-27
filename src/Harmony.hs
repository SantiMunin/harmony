module Main where

import qualified Generation.OutputGenerator as OG
import           Language.Abs
import           Language.ErrM
import           Language.Par
import qualified StaticCheck                as SC
import           System.Environment         (getArgs)

main :: IO ()
main = do
  args <- getArgs
  case args of
       [file] -> do
         contents <- readFile file
         case pSpecification (myLexer contents) of
           Bad err -> error $ show err
           Ok tree -> case SC.staticCheck tree of
                        Bad err -> error $ show err
                        Ok _ -> generateOutput tree
       _ -> error "Usage: harmony <source_file>"
  return ()

  -- TODO: use flags to collect information for client and server, tests should be always the same
generateOutput :: Specification -> IO ()
generateOutput = OG.generateOutput "/tmp/harmony_output" (OG.createGenInfo files templates)
  where
    files = []
    templates = [ ("templates/server/js/server.tpl", "js")
                , ("templates/server/js/package.tpl", "json") ]

