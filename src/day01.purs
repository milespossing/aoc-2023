module Day01 where

import Prelude
import Data.Array as Array
import Data.String (Pattern(..), split, null, uncons)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String.CodePoints (CodePoint, singleton)

asNumber :: CodePoint -> Maybe Int
asNumber cp = fromString(singleton cp)

getFirstLast :: String -> Int
getFirstLast line = aux line (Tuple Nothing Nothing) where
  aux :: String -> Tuple (Maybe Int) (Maybe Int) -> Int
  aux s acc = case uncons s of
    Nothing -> case acc of
      Tuple Nothing _ -> 0
      Tuple (Just a) Nothing -> a * 10 + a
      Tuple (Just a) (Just b) -> a * 10 + b
    Just { head, tail } -> case asNumber head of 
      Nothing -> aux tail acc
      Just i -> case acc of
        Tuple Nothing _ -> aux tail (Tuple (Just i) Nothing)
        Tuple f _ -> aux tail (Tuple f (Just i))

solve01 :: String -> Int
solve01 = (
  split (Pattern "\n") >>>
  Array.filter (not <<< null) >>>
  map getFirstLast >>>
  Array.foldl (\a -> \b -> a + b) 0
)
  
