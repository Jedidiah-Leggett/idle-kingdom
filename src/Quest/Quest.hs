module Quest.Quest
( Quest (..)
, initQuest
, processQuest
) where

import Data.Text (Text, unpack)

import qualified Kingdom
import qualified Peasants
import qualified Resource

data Quest =
  Quest
  { description :: Text
  , requirements :: Kingdom.Kingdom -> Bool
  , reward :: Kingdom.Kingdom -> Kingdom.Kingdom
  , rewardText :: Text
  , nextQuest :: Quest
  }

instance Show Quest where
  show = unpack . ("Quest: " <>) . description

processQuest :: Kingdom.Kingdom -> Quest -> Either () (Kingdom.Kingdom, Quest)
processQuest k q =
  if requirements q k
  then Right (reward q k, nextQuest q)
  else Left ()

initQuest :: Quest
initQuest =
      Quest
      { description = "Build a farm"
      , requirements = (>= 2) . Resource.productionCount . Kingdom.food
      , reward = id
      , rewardText = "Moooo!"
      , nextQuest = lumberQuest
      }

lumberQuest :: Quest
lumberQuest =
      Quest
      { description = "Build a lumber mill"
      , requirements = (>= 2) . Resource.productionCount . Kingdom.wood
      , reward = id
      , rewardText = "Brown chicken brown cow!"
      , nextQuest = populationQuest
      }

populationQuest :: Quest
populationQuest =
      Quest
      { description = "Increase Population to 10"
      , requirements = (>= 10) . Peasants.populationTotal . Kingdom.population
      , reward = id
      , rewardText = "Brown chicken brown cow!"
      , nextQuest = emptyQuest
      }

emptyQuest :: Quest
emptyQuest =
  Quest
  { description = "Thanks for playing!"
  , requirements = const False
  , reward = id
  , rewardText = ""
  , nextQuest = emptyQuest
  }