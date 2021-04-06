module Farms
( Farms
, initFarms
, farmProduction
, incrementFarms
, farmNetProduction
) where

import Data.Int (Int32)

import qualified Peasants
import qualified Resource

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

farmProduction :: Farms -> Resource.Food
farmProduction = Resource.Food . (*5) . count

farmNetProduction :: Farms -> Peasants.Peasants -> Resource.Food
farmNetProduction f p = farmProduction f - (Resource.Food $ Peasants.peasantPopulation p)

incrementFarms :: Farms -> Farms
incrementFarms f = f { count = count f + 1 }
