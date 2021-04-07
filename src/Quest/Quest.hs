module Quest.Quest
( Quest (..)
, initQuest
, processQuest
) where

import Data.Text (Text, unpack)

import qualified Kingdom
import qualified Peasants

data Quest =
  Quest
  { description :: Text
  , requirements :: Kingdom.Kingdom -> Bool
  , reward :: Kingdom.Kingdom -> Kingdom.Kingdom
  , rewardText :: Text
  }

instance Show Quest where
  show = unpack . ("Quest: " <>) . description

initQuest :: Quest
initQuest =
      Quest
      { description = "Increase Population to 10"
      , requirements = (== 10) . Peasants.populationTotal . Kingdom.population
      , reward = id
      , rewardText = "Brown chicken brown cow!"
      }

processQuest :: Kingdom.Kingdom -> Quest -> Either () (Kingdom.Kingdom, Quest)
processQuest k q =
  if requirements q k
  then Right (reward q k, emptyQuest)
  else Left ()

emptyQuest :: Quest
emptyQuest =
  Quest
  { description = "Thanks for playing!"
  , requirements = const False
  , reward = id
  , rewardText = ""
  }