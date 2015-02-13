module Main where

import qualified Data.Set           as Set
import           System.Environment (getArgs)

import           Language.Abs
import           Language.ErrM
import           Language.Lex
import           Language.Par
import           Language.Print
import           StaticCheck

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
       args -> error "Usage: harmony <source_file>"
  return ()

generateOutput :: Specification -> IO ()
generateOutput (Spec name version enums structs resource) = putStrLn "A"
