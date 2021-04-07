{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Resource.Type.Wood.WoodEntity
( Wood(..)
, WoodStorage
, initWoodStorage
, processWood
) where

import Data.Int(Int32)

import qualified Resource.Resource as Resource

newtype Wood = Wood Int32
  deriving (Eq, Integral, Real, Enum, Ord, Num)

instance Show Wood where
  show (Wood f) = show f

type WoodStorage = Resource.Resource Wood

initWoodStorage :: WoodStorage
initWoodStorage = Resource.initResource (Wood 0) (Wood 100)

processWood :: WoodStorage -> Wood -> WoodStorage
processWood storage availableWood =
  let
    negativeWood = availableWood < 0
    maxWood = availableWood > 0
    newWood
      | availableWood < 0 = 0
      | availableWood > Resource.maxAmount storage = Resource.maxAmount storage
      | otherwise = availableWood
  in
    storage { Resource.amount = newWood }
