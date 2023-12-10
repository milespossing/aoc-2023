module Day03 where

import Prelude
import Utils
import Effect (Effect)
import Effect.Console (log)
import Data.Either (Either(..))
import Data.String as Str
import Data.String.CodeUnits (charAt, uncons)
import Data.CodePoint.Unicode (isDecDigit, isSymbol, decDigitToInt)
import Data.Array as Array
import Data.Maybe (Maybe(..))

pointIsSymbol :: Array String -> Int -> Int -> Boolean
pointIsSymbol lines lineNum col = case Array.index lines lineNum of
  Nothing -> false
  Just line -> case charAt col line of
    Nothing -> false
    Just '.' -> false
    Just c -> not $ isDecDigit $ Str.codePointFromChar c

hasNeighbor :: Array String -> Int -> Int -> Boolean
hasNeighbor lines line col =
  -- left
  pointIsSymbol lines line (col - 1)
    -- right
    
    || pointIsSymbol lines line (col + 1)
    -- above left
    
    || pointIsSymbol lines (line - 1) (col - 1)
    -- bottom left
    
    || pointIsSymbol lines (line + 1) (col - 1)
    -- above
    
    || pointIsSymbol lines (line - 1) col
    -- below
    
    || pointIsSymbol lines (line + 1) col
    -- above right
    
    || pointIsSymbol lines (line - 1) (col + 1)
    -- below right
    
    || pointIsSymbol lines (line + 1) (col + 1)

getDigit :: Char -> Int
getDigit c = case decDigitToInt $ Str.codePointFromChar c of
  Nothing -> 0
  Just i -> i

charIsDec :: Char -> Boolean
charIsDec = isDecDigit <<< Str.codePointFromChar

getNumbersForLine :: Array String -> Int -> String -> Array Int
getNumbersForLine lines lineNumber line = aux 0 line 0 false []
  where
  aux :: Int -> String -> Int -> Boolean -> Array Int -> Array Int
  aux i s current currentHasNeighbor acc = case uncons s of
    Nothing
      | current > 0 && currentHasNeighbor -> Array.snoc acc current
    Nothing -> acc
    -- for digits
    Just { head: c, tail }
      | charIsDec c -> aux (i + 1) tail (current * 10 + getDigit c) (currentHasNeighbor || (hasNeighbor lines lineNumber i)) acc
    -- for non digits when the last _was_ a digit
    Just { head: _, tail }
      | current > 0 && currentHasNeighbor -> aux (i + 1) tail 0 false (Array.snoc acc current)
    -- throw away and move forward
    Just { head: _, tail } -> aux (i + 1) tail 0 false acc

getNumbers :: Array String -> Array Int
getNumbers lines = Array.concat $ Array.mapWithIndex (getNumbersForLine lines) lines

solve1 :: String -> Effect (Either String Int)
solve1 file = do
  let
    lines = getLines file
  let
    numbers = getNumbers lines
  -- log (show numbers)
  let
    result = Array.foldl (\a -> \b -> a + b) 0 numbers
  pure (Right result)

solve2 :: String -> Either String Int
solve2 _ = Left "Not Implemented"
