module Day01 where

import Prelude
import Data.Array as Array
import Data.String (Pattern(..), split, null, uncons, drop, take)
import Data.String.Utils (startsWith)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Data.String.CodePoints (CodePoint, singleton)

asNumber :: CodePoint -> Maybe Int
asNumber cp = fromString(singleton cp)

getStartChars :: String -> Maybe (Tuple Int String)
getStartChars "" = Nothing
getStartChars s = case true of
  _ | startsWith "one" s -> Just (Tuple 1 (drop 1 s))
  _ | startsWith "two" s -> Just (Tuple 2 (drop 1 s))
  _ | startsWith "three" s -> Just (Tuple 3 (drop 1 s))
  _ | startsWith "four" s -> Just (Tuple 4 (drop 1 s))
  _ | startsWith "five" s -> Just (Tuple 5 (drop 1 s))
  _ | startsWith "six" s -> Just (Tuple 6 (drop 1 s))
  _ | startsWith "seven" s -> Just (Tuple 7 (drop 1 s))
  _ | startsWith "eight" s -> Just (Tuple 8 (drop 1 s))
  _ | startsWith "nine" s -> Just (Tuple 9 (drop 1 s))
  _ -> Nothing

getFirstLast :: String -> Int
getFirstLast line = aux line (Tuple Nothing Nothing) where
  aux :: String -> Tuple (Maybe Int) (Maybe Int) -> Int
  aux "" acc = case acc of
      Tuple Nothing _ -> 0
      Tuple (Just a) Nothing -> a * 10 + a
      Tuple (Just a) (Just b) -> a * 10 + b
  aux s acc = let
      head = take 1 s
      tail = drop 1 s in
        case fromString head of
          Nothing -> case getStartChars s of
            Nothing -> aux tail acc
            Just (Tuple i t) -> aux t (case acc of
              Tuple Nothing _ -> Tuple (Just i) Nothing
              Tuple f _ -> Tuple f (Just i))
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
  
