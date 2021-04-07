module Game
( Game
, initGame
, addFarm
, addLumberMill
, addPeasant
, processGame
) where

import Data.Text (intercalate, unpack)

import qualified Kingdom
import qualified Quest
import qualified Utils

data Game =
  Game
  { kingdom :: Kingdom.Kingdom
  , quest :: Quest.Quest
  }

instance Show Game where
  show g =
    unpack $ Utils.showText (quest g)
    <> "\n\n"
    <> Utils.showText (kingdom g)

initGame :: Game
initGame =
  Game
  { kingdom = Kingdom.initKingdom
  , quest = Quest.initQuest
  }

addFarm :: Game -> Game
addFarm g = g { kingdom = Kingdom.addFarm $ kingdom g }

addLumberMill :: Game -> Game
addLumberMill g = g { kingdom = Kingdom.addLumberMill $ kingdom g }

addPeasant :: Game -> Game
addPeasant g = g { kingdom = Kingdom.addPeasant $ kingdom g }

processGame :: Game -> Game
processGame g =
  let
    (k', q') = case Quest.processQuest (kingdom g) (quest g) of
      Left _ -> (kingdom g, quest g)
      Right result -> result
  in
    g 
    { kingdom = Kingdom.processKingdom k' 
    , quest = q'
    }

-- processQuest :: 