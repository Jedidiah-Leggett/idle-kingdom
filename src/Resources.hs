{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Resources
( Food(Food)
, Wood(Wood)
, processFood
, processWood
)
where

import Data.Int(Int32)
import Data.Text(Text)

import qualified Utils

newtype Food = Food Int32
  deriving (Eq, Integral, Real, Enum, Ord, Num)

instance Show Food where
  show (Food f) = show f

processFood :: Food -> Food
processFood availableFood =
  if availableFood < 0
  then 0
  else availableFood

newtype Wood = Wood Int32
  deriving (Eq, Integral, Real, Enum, Ord, Num)

instance Show Wood where
  show (Wood f) = show f

processWood :: Wood -> Wood
processWood availableWood =
  if availableWood < 0
  then 0
  else availableWood

data Storage a =
  Storage
  { amountStored :: a
  , maxAmount :: Int32
  , count :: Int32
  }
