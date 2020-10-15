module Main where

import PrologParser
import System.IO

runParser :: String -> IO ()
runParser str =
  case parseModul' str of
    Left err -> print err
    Right r -> print r

-- parseFromFile :: FilePath -> IO ()
-- parseFromFile path = do
--   input <- readFile path
--   case parseProgram input of
--     Left err -> print err
--     Right r -> do
--       writeFile (path ++ ".out") (show r)

main :: IO ()
main = do
  putStrLn ""

  runParser "module name_123 ."

-- writeFile "input.txt" "a+2^3*4"
-- parseFromFile "input.txt"