{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Resource.Type.Food
( Food(..)
, FoodStorage
, initFoodStorage
, processFood
) where

import Data.Int(Int32)

import qualified Resource.Resource as Resource

newtype Food = Food Int32
  deriving (Eq, Integral, Real, Enum, Ord, Num)

instance Show Food where
  show (Food f) = show f

type FoodStorage = Resource.Resource Food

initFoodStorage :: FoodStorage
initFoodStorage = Resource.initResource (Food 0) (Food 20)

processFood :: FoodStorage -> Food -> FoodStorage
processFood storage availableFood =
  let
    negativeFood = availableFood < 0
    maxFood = availableFood > 0
    newFood
      | availableFood < 0 = 0
      | availableFood > Resource.maxAmount storage = Resource.maxAmount storage
      | otherwise = availableFood
  in
    storage { Resource.amount = newFood }
