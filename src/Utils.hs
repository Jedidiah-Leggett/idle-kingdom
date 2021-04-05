module Utils where

import Data.Text (Text, pack)

showText :: Show a => a -> Text
showText = pack . show

showWithSign :: (Show a, Num a, Ord a) => a -> Text
showWithSign num =
  if num >= 0
  then pack $ "+" <> show num
  else showText num