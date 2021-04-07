module Main where

import System.Console.ANSI (clearScreen)
import System.IO (hWaitForInput, stdin, hGetLine)

import qualified Kingdom

main :: IO ()
main =
  run Kingdom.initKingdom

run :: Kingdom.Kingdom -> IO ()
run k = do
  clearScreen
  print k
  putStrLn "(F) Make Farm {10 wood} | (L) Make Lumber Mill {100 wood} | (P) Make Peasant {10 food}"
  -- action <- getLine
  getAction <- hWaitForInput stdin 1000
  if getAction
  then do
    action <- hGetLine stdin
    case action of
      "F" -> run $ Kingdom.addFarm k
      "f" -> run $ Kingdom.addFarm k
      "P" -> run $ Kingdom.addPeasant k
      "p" -> run $ Kingdom.addPeasant k
      "L" -> run $ Kingdom.addLumberMill k
      "l" -> run $ Kingdom.addLumberMill k
      "exit" -> pure ()
      "E" -> pure ()
      "e" -> pure ()
      "" -> run $ Kingdom.processKingdom k
      _ -> run k
  else
    run $ Kingdom.processKingdom k
