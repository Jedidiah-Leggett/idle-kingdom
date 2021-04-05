{-# LANGUAGE OverloadedStrings #-}

module Kingdom
( Kingdom
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
import qualified Resources

data Kingdom =
  Kingdom
  { population :: Peasants.Peasants
  , farms :: Farms.Farms
  , lumberMills :: LumberMills.LumberMills
  , food :: Resources.Food
  , wood :: Resources.Wood
  }

instance Show Kingdom where
  show k = unpack $
       "Food: " <> "(" <> Utils.showWithSign foodConsumption <> ") " <> food'
    <> " | Wood: " <> "(" <> Utils.showWithSign lumberConsumption <> ") " <> wood'
    <> " | Population: " <> population'
    <> " | Farms: " <> farms'
    <> " | Lumber Mills: " <> lumberMills'
     where
       food' = Utils.showText (food k)
       wood' = Utils.showText (wood k)
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
  , food = 0
  , wood = 0
  }

addFarm :: Kingdom -> Kingdom
addFarm k =
  if wood k < 10 then k
  else
    k
    { farms = Farms.incrementFarms (farms k)
    , wood = wood k - 10
    }

addLumberMill :: Kingdom -> Kingdom
addLumberMill k = 
  if wood k < 100 then k
  else
    k
    { lumberMills = LumberMills.incrementLumberMills $ lumberMills k
    , wood = wood k - 100
    }

addPeasant :: Kingdom -> Kingdom
addPeasant k =
  if food k < 10 then k
  else
    k
    { population = Peasants.birthOne $ population k
    , food = food k - 10
    }

processKingdom :: Kingdom -> Kingdom
processKingdom k =
  let
    availableFood = (+ food k) $ Farms.farmNetProduction (farms k) (population k)
    newFood = Resources.processFood availableFood
    newPopulation = Peasants.processPopulation (population k) availableFood
    availableWood = (+ wood k) $ LumberMills.lumberMillNetProduction (lumberMills k)
    newWood = Resources.processWood availableWood
  in
    k
    { food = newFood
    , population = newPopulation
    , wood = newWood
    }
