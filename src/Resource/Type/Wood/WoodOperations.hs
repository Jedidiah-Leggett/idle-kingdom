module Resource.Type.Wood.WoodOperations
( lumberMillProduction
, incrementLumberMills
, nextLumberMillCost
) where


import qualified Resource.Type.Food.FoodEntity as Resource
import qualified Resource.Resource as Resource
import qualified Resource.Type.Wood.WoodEntity as Resource
import qualified Utils

lumberMillProduction :: Resource.WoodStorage -> Resource.Wood
lumberMillProduction = Resource.Wood . (*5) . Resource.productionCount

incrementLumberMills :: Resource.WoodStorage -> Resource.WoodStorage
incrementLumberMills = Resource.incrementProduction

nextLumberMillCost :: Resource.WoodStorage -> Resource.Wood
nextLumberMillCost fs = Resource.Wood $ Utils.fib (Resource.productionCount fs) * 20
