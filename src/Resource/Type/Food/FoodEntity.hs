{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Resource.Type.Food.FoodEntity
( Food(..)
, FoodStorage
, initFoodStorage
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
