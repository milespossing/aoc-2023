module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Day01 as Day01
import Day02 as Day02
import Day03 as Day03

readFile :: String -> Effect String
readFile = readTextFile UTF8

main :: Effect Unit
main = do
  file1 <- readFile "inputs/day01.txt"
  log ("Day 1: " <> show (Day01.solve file1))
  file2 <- readFile "inputs/day02.txt"
  log ("Day 2.1: " <> show (Day02.solve file2))
  log ("Day 2.2: " <> show (Day02.solve02 file2))
  file3 <- readFile "inputs/day03.txt"
  log ("Day 3.1: " <> show (Day03.solve1 file3))
  log ("Day 3.2: " <> show (Day03.solve2 file3))
