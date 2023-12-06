module Day03 where

import Prelude
import Data.Either (Either(..), note)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as Str
import Data.String.CodePoints (toCodePointArray, fromCodePointArray)
import Data.String.CodeUnits (charAt)
import Data.String.Regex (test, regex, noFlags)

type Schematic = Array String

type NumberPoint = { line :: Int, start :: Int, len :: Int, val :: Int }
numberPoint :: Int -> Int -> Int -> Int -> NumberPoint
numberPoint line start len val = { line: line, start: start, len: len, val: val }

type SchematicCheck = Schematic -> NumberPoint -> Boolean

checkLeft :: SchematicCheck
checkLeft _ { start: 0 } = false
checkLeft s { line, start } = case Array.index s line of
  Nothing -> false
  Just l -> case charAt (start - 1) l of
    Nothing -> false
    Just '.' -> false
    Just _ -> true
checkRight :: SchematicCheck
checkRight s { line, start, len } = case Array.index s line of
  Nothing -> false
  Just l -> case charAt (start + len + 1) l of
    Nothing -> false
    Just '.' -> false
    Just _ -> true

getCharsAtLine :: Schematic -> Int -> Int -> Int -> Maybe String
getCharsAtLine s line start len = do
  l <- Array.index s line
  let a = toCodePointArray l
  let h = Array.drop start a
  let c = Array.take len h
  pure (fromCodePointArray c)

findFirstNumber :: String -> Maybe Int

getNextNumber :: String -> Array NumberPoint
getNextNumber s = do
  
  regex "^(\\d+)" noFlags >>>

getFromLine :: Int -> String -> Array NumberPoint
getFromLine lineNumber line = aux (Tuple line 0) [] where
  aux :: Int -> Int -> Tuple String Int -> Array NumberPoint -> Array NumberPoint
  aux _ (Tuple "" _) acc -> acc
  aux 
do
  let nextStr = match (regex "^(\\d+)" noFlags) line

scan :: Schematic -> Array NumberPoint
scan s = Array.concat $
         Array.mapWithIndex getFromLine s
  

hasChar :: Schematic -> NumberPoint -> Boolean
hasChar s n = checkLeft s n && checkRight s n

filterPoints :: Schematic -> Array NumberPoint -> Array NumberPoint
filterPoints s = Array.filter (hasChar s)

sumNumbers :: Array NumberPoint -> Int
sumNumbers = Array.foldl (\acc -> \point -> acc + point.val) 0

solve1 :: String -> Either String Int
solve1 file = do
  let s = Array.filter (not <<< Str.null) $ Str.split (Str.Pattern "\n") file
  nums <- scan s
  pure (sumNumbers $ filterPoints s nums)

solve2 :: String -> Either String Int
solve2 _ = Left "Not Implemented"
