{-# LANGUAGE DataKinds     #-}

module Resource.Resource
( Resource(..)
, initResource
, consumeResource
, incrementProduction
) where

import Data.Int (Int32)
import Data.Text (Text)

data Resource a =
  Resource
  { amount :: a
  , maxAmount :: a
  , storageLevel :: Int32
  , productionCount :: Int32
  }

initResource :: Num a => a -> a -> Resource a
initResource a max =
  Resource
  { amount = a
  , maxAmount = max
  , storageLevel = 1
  , productionCount = 1
  }

consumeResource :: (Num a, Ord a) => Resource a -> a -> Either () (Resource a)
consumeResource r a =
  if amount r >= a
  then Right $ r { amount = amount r - a }
  else Left ()

incrementProduction :: Integral a => Resource a -> Resource a
incrementProduction r =
  r
  { maxAmount = ((*3) $ maxAmount r) `quot` 2
  , productionCount = productionCount r + 1
  }
