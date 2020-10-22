module Main where

import System.IO
import System.Environment 
import Data.List
import Text.ParserCombinators.Parsec as All hiding (parseFromFile)
import PrologParser

parseFromFile :: Show a => Parser a -> FilePath -> IO ()
parseFromFile parser path = do
  input <- readFile path
  case parseString parser input of
    Left err -> print err
    Right r -> do
      writeFile (path ++ ".out") (show r)

main :: IO ()
main = do
  args <- getArgs
  let flag = head args 
  let file = head $ tail args
  case flag of
    "--atom" -> parseFromFile atom file 
    "--typeexpr" -> parseFromFile typeExpr file 
    "--type" -> parseFromFile typ file 
    "--module" -> parseFromFile parseModule file 
    "--relation" -> parseFromFile relation file 
    "--list" -> parseFromFile list file 
    "--prog" -> parseFromFile prog file

