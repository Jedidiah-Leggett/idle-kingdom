module Main where

import Data.Text (unpack)
import System.Console.ANSI (clearScreen)
import qualified System.IO as IO

import qualified Kingdom
import qualified Resource
import qualified Game
import qualified Utils

main :: IO ()
main =
  run Game.initGame

run :: Game.Game -> IO ()
run g = do
  clearScreen
  putStrLn "\r"
  print g
  putStrLn . unpack $ "(F) Make Farm {" <> Utils.showText nextFarmCost <> " wood} | (L) Make Lumber Mill {" <> Utils.showText  nextLumberMillCost <>" wood} | (P) Make Peasant {10 food}"
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
  where
    nextFarmCost = Resource.nextFarmCost . Kingdom.food . Game.kingdom $ g
    nextLumberMillCost = Resource.nextLumberMillCost . Kingdom.wood . Game.kingdom $ g
