module Main where

import System.Environment

import Language.ErrM
import Language.Abs
import Language.Lex
import Language.Par
import Language.Print

main :: IO ()
main = do 
  args <- getArgs
  case args of
       [file] -> do
         contents <- readFile file
         case pSpecification (myLexer contents) of
           Bad err -> error $ show err 
           Ok tree -> putStrLn $ printTree tree
       args -> error $ "Usage: harmony <source_file>" 
  return ()

