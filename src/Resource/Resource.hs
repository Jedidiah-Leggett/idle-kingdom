{-# LANGUAGE DataKinds     #-}

module Resource.Resource
( Resource(..)
, initResource
, consumeResource
) where

import Data.Int (Int32)
import Data.Text (Text)

data Resource a =
  Resource
  { amount :: a
  , maxAmount :: a
  , storageLevel :: Int32
  }

initResource :: Num a => a -> a -> Resource a
initResource a max =
  Resource
  { amount = a
  , maxAmount = max
  , storageLevel = 1
  }

consumeResource :: (Num a, Ord a) => Resource a -> a -> Either () (Resource a)
consumeResource r a =
  if amount r >= a
  then Right $ r { amount = amount r - a }
  else Left ()
