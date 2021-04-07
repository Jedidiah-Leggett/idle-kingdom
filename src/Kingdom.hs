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

import qualified Peasants
import qualified Utils
import qualified Resource

data Kingdom =
  Kingdom
  { population :: Peasants.Peasants
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
       farmCount = Resource.productionCount (food k)
       lumberMillCount = Resource.productionCount (wood k)
       foodConsumption = netFarmProduction k
       lumberConsumption = netLumberMillProduction k
       population' = Utils.showText (population k)
       farms' = Utils.showText farmCount
       lumberMills' = Utils.showText lumberMillCount

initKingdom :: Kingdom
initKingdom =
  Kingdom
  { population = Peasants.initPeasants
  , food = Resource.initFoodStorage
  , wood = Resource.initWoodStorage
  }

addFarm :: Kingdom -> Kingdom
addFarm k =
  case Resource.consumeResource (wood k) (Resource.nextFarmCost $ food k) of
    Left _ -> k
    Right newWood ->
      k
      { food = Resource.incrementFarm (food k)
      , wood = newWood
      }

addLumberMill :: Kingdom -> Kingdom
addLumberMill k = 
  case Resource.consumeResource (wood k) (Resource.nextLumberMillCost $ wood k) of
    Left _ -> k
    Right newWood ->
      k
      { wood = Resource.incrementLumberMills newWood
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
    availableFood = (+ (Resource.amount $ food k)) $ netFarmProduction k
    newFood = Resource.processFood (food k) availableFood
    newPopulation = Peasants.processPopulation (population k) availableFood
    availableWood = (+ (Resource.amount $ wood k)) $ netLumberMillProduction k
    newWood = Resource.processWood (wood k) availableWood
  in
    k
    { food = newFood
    , population = newPopulation
    , wood = newWood
    }

netFarmProduction :: Kingdom -> Resource.Food
netFarmProduction k = Resource.farmProduction (food k) - (Resource.Food $ Peasants.peasantPopulation (population k))

netLumberMillProduction :: Kingdom -> Resource.Wood
netLumberMillProduction = Resource.lumberMillProduction . wood
