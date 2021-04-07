{-# LANGUAGE OverloadedStrings #-}

module Kingdom
( Kingdom(..)
, processKingdom
, initKingdom
, addFarm
, addPeasant
, addLumberMill
)
where

import Data.Int (Int32)
import Data.Text (Text, unpack)

import qualified Farms
import qualified LumberMills
import qualified Peasants
import qualified Utils
import qualified Resource

data Kingdom =
  Kingdom
  { population :: Peasants.Peasants
  , farms :: Farms.Farms
  , lumberMills :: LumberMills.LumberMills
  , food :: Resource.FoodStorage
  , wood :: Resource.WoodStorage
  }

instance Show Kingdom where
  show k = unpack $
       "Food: " <> "(" <> Utils.showWithSign foodConsumption <> ") " <> foodAmount <> "/" <> foodMax
    <> " | Wood: " <> "(" <> Utils.showWithSign lumberConsumption <> ") " <> woodAmount <> "/" <> woodMax
    <> " | Population: " <> population'
    <> " | Farms: " <> farms'
    <> " | Lumber Mills: " <> lumberMills'
     where
       foodAmount = Utils.showText . Resource.amount $ food k
       foodMax = Utils.showText . Resource.maxAmount $ food k
       woodAmount = Utils.showText . Resource.amount $ wood k
       woodMax = Utils.showText . Resource.maxAmount $ wood k
       foodConsumption = Farms.farmNetProduction (farms k) (population k)
       lumberConsumption = LumberMills.lumberMillNetProduction (lumberMills k)
       population' = Utils.showText (population k)
       farms' = Utils.showText (farms k)
       lumberMills' = Utils.showText (lumberMills k)

initKingdom :: Kingdom
initKingdom =
  Kingdom
  { population = Peasants.initPeasants
  , farms = Farms.initFarms
  , lumberMills = LumberMills.initLumberMills
  , food = Resource.initFoodStorage
  , wood = Resource.initWoodStorage
  }

addFarm :: Kingdom -> Kingdom
addFarm k =
  case Resource.consumeResource (wood k) 10 of
    Left _ -> k
    Right newWood ->
      k
      { farms = Farms.incrementFarms (farms k)
      , wood = newWood
      }

addLumberMill :: Kingdom -> Kingdom
addLumberMill k = 
  case Resource.consumeResource (wood k) 100 of
    Left _ -> k
    Right newWood ->
      k
      { lumberMills = LumberMills.incrementLumberMills $ lumberMills k
      , wood = newWood
      }

addPeasant :: Kingdom -> Kingdom
addPeasant k =
  case Resource.consumeResource (food k) 10 of
    Left _ -> k
    Right newFood ->
      k
      { population = Peasants.birthOne $ population k
      , food = newFood
      }

processKingdom :: Kingdom -> Kingdom
processKingdom k =
  let
    availableFood = (+ (Resource.amount $ food k)) $ Farms.farmNetProduction (farms k) (population k)
    newFood = Resource.processFood (food k) availableFood
    newPopulation = Peasants.processPopulation (population k) availableFood
    availableWood = (+ (Resource.amount $ wood k)) $ LumberMills.lumberMillNetProduction (lumberMills k)
    newWood = Resource.processWood (wood k) availableWood
  in
    k
    { food = newFood
    , population = newPopulation
    , wood = newWood
    }
