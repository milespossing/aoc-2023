module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

import Day01 (solve01)

main :: Effect Unit
main = do
  file1 <- readTextFile UTF8 "inputs/day01.txt"
  log ("Day 1: " <> show (solve01 file1))

