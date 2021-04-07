module LumberMills
( LumberMills
, initLumberMills
, lumberMillProduction
, incrementLumberMills
, lumberMillNetProduction
) where

import Data.Int (Int32)

import qualified Peasants
import qualified Resource

newtype LumberMills =
  LumberMills
  { count :: Int32
  }

instance Show LumberMills where
  show = show . count

initLumberMills :: LumberMills
initLumberMills =
  LumberMills
  { count = 1
  }

lumberMillProduction :: LumberMills -> Resource.Wood
lumberMillProduction = Resource.Wood . (*5) . count

lumberMillNetProduction :: LumberMills -> Resource.Wood
lumberMillNetProduction = lumberMillProduction

incrementLumberMills :: LumberMills -> LumberMills
incrementLumberMills f = f { count = count f + 1 }
