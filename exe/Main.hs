module Main where

import System.Console.ANSI (clearScreen)
import qualified System.IO as IO

import qualified Kingdom
import qualified Game

main :: IO ()
main =
  run Game.initGame

run :: Game.Game -> IO ()
run g = do
  clearScreen
  print g
  putStrLn "(F) Make Farm {10 wood} | (L) Make Lumber Mill {100 wood} | (P) Make Peasant {10 food}"
  -- action <- getLine
  getAction <- IO.hWaitForInput IO.stdin 1000
  if getAction
  then do
    action <- IO.hGetChar IO.stdin
    case action of
      'F' -> run $ Game.addFarm g
      'f' -> run $ Game.addFarm g
      'P' -> run $ Game.addPeasant g
      'p' -> run $ Game.addPeasant g
      'L' -> run $ Game.addLumberMill g
      'l' -> run $ Game.addLumberMill g
      '\27' -> pure ()
      'E' -> pure ()
      'e' -> pure ()
      _ -> run g
  else
    run $ Game.processGame g
