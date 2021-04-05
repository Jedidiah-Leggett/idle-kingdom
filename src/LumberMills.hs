module LumberMills
( LumberMills
, initLumberMills
, lumberMillProduction
, incrementLumberMills
, lumberMillNetProduction
) where

import Data.Int (Int32)

import qualified Peasants
import qualified Resources

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

lumberMillProduction :: LumberMills -> Resources.Wood
lumberMillProduction = Resources.Wood . (*2) . count

lumberMillNetProduction :: LumberMills -> Resources.Wood
lumberMillNetProduction = lumberMillProduction

incrementLumberMills :: LumberMills -> LumberMills
incrementLumberMills f = f { count = count f + 1 }
