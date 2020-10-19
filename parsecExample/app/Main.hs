module Main where

import PrologParser
import System.IO

runParser :: String -> IO ()
runParser str =
  case parseProgram str of
    Left err -> print err
    Right r -> print r

parseFromFile :: FilePath -> IO ()
parseFromFile path = do
  input <- readFile path
  case parseProgram input of
    Left err -> print err
    Right r -> do
      writeFile (path ++ ".out") (show r)

main :: IO ()
main = do
  putStrLn ""

  runParser "module m. type tap (A -> o) -> lst A -> o. var k p :- (pak) ; (p , k)."

-- writeFile "input.txt" "a+2^3*4"
-- parseFromFile "input.txt"