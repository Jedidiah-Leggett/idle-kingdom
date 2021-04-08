module Resource.Type.Food.FoodOperations
( processFood
, farmProduction
, incrementFarm
, nextFarmCost
) where

import qualified Resource.Type.Food.FoodEntity as Resource
import qualified Resource.Resource as Resource
import qualified Resource.Type.Wood.WoodEntity as Resource
import qualified Utils

processFood :: Resource.FoodStorage -> Resource.Food -> Resource.FoodStorage
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

farmProduction :: Resource.FoodStorage -> Resource.Food
farmProduction = Resource.Food . (*3) . Resource.productionCount

incrementFarm :: Resource.FoodStorage -> Resource.FoodStorage
incrementFarm = Resource.incrementProduction

nextFarmCost :: Resource.FoodStorage -> Resource.Wood
nextFarmCost fs = Resource.Wood $ Utils.fib (Resource.productionCount fs) * 10
