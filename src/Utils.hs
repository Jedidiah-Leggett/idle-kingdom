module Utils where

import Data.Int (Int32)
import Data.Text (Text, pack)

showText :: Show a => a -> Text
showText = pack . show

showWithSign :: (Show a, Num a, Ord a) => a -> Text
showWithSign num =
  if num >= 0
  then pack $ "+" <> show num
  else showText num

fib :: Int32 -> Int32
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
