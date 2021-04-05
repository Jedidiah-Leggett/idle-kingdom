module Farms
( Farms
, initFarms
, farmProduction
, incrementFarms
, farmNetProduction
) where

import Data.Int (Int32)

import qualified Peasants
import qualified Resources

newtype Farms =
  Farms
  { count :: Int32
  }

instance Show Farms where
  show = show . count

initFarms :: Farms
initFarms =
  Farms
  { count = 1
  }

farmProduction :: Farms -> Resources.Food
farmProduction = Resources.Food . (*2) . count

farmNetProduction :: Farms -> Peasants.Peasants -> Resources.Food
farmNetProduction f p = farmProduction f - (Resources.Food $ Peasants.peasantPopulation p)

incrementFarms :: Farms -> Farms
incrementFarms f = f { count = count f + 1 }
