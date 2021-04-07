module Peasants
( Peasants
, birthOne
, killOne
, initPeasants
, peasantPopulation
, processPopulation
, populationTotal
) where

import Data.Int (Int32)

import qualified Resource

newtype Peasants =
  Peasants
  { count :: Int32
  }

instance Show Peasants where
  show = show . count

initPeasants :: Peasants
initPeasants =
  Peasants
  { count = 1
  }

birthOne :: Peasants -> Peasants
birthOne p = p { count = count p + 1 }

killOne :: Peasants -> Peasants
killOne p = p { count = count p - 1 }

peasantPopulation :: Peasants -> Int32
peasantPopulation = count

processPopulation :: Peasants -> Resource.Food -> Peasants
processPopulation p food =
  if food < 0
  then last . take ((+1) . abs $ fromIntegral food) $ iterate killOne p
  else p

populationTotal :: Peasants -> Int32
populationTotal = count
