module Day02 where

import Prelude
import Data.Array as Array
import Data.Traversable (traverse)
import Data.Foldable (all)
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.String as Str
import Data.Tuple (Tuple(..))
import Data.Maybe (Maybe(..), fromMaybe)

data Color
  = Red
  | Blue
  | Green

type CubeCombo
  = Tuple Int Color

type CubeReveal
  = Array CubeCombo

type Game
  = { number :: Int, reveals :: Array CubeReveal }

parseColor :: String -> Maybe Color
parseColor s = case Str.trim s of
  "red" -> Just Red
  "green" -> Just Green
  "blue" -> Just Blue
  _ -> Nothing

parseCubeComb :: String -> Maybe CubeCombo
parseCubeComb s = do
  let
    parts = Str.split (Str.Pattern " ") s
  case parts of
    [ numStr, colorStr ] -> do
      num <- fromString numStr
      color <- parseColor colorStr
      pure (Tuple num color)
    _ -> Nothing

parseCubeReveal :: String -> Maybe CubeReveal
parseCubeReveal s = traverse parseCubeComb $ Str.split (Str.Pattern ", ") $ Str.trim s

parseGame :: String -> Either String Game
parseGame s = do
  case Str.split (Str.Pattern ": ") s of
    [ numStr, games ] -> do
      let
        gameString = fromMaybe "" $ Array.last $ Str.split (Str.Pattern " ") $ Str.trim numStr
      gameNum <- note "Invalid game number" $ fromString gameString
      reveals <- note "Invalid cubes" $ traverse parseCubeReveal $ Str.split (Str.Pattern "; ") $ Str.trim games
      pure { number: gameNum, reveals: reveals }
    _ -> Left "Invalid Game Format"

comboIsValid :: CubeCombo -> Boolean
comboIsValid c = case c of
  Tuple n Red -> n <= 12
  Tuple n Green -> n <= 13
  Tuple n Blue -> n <= 14

revealIsValid :: CubeReveal -> Boolean
revealIsValid = all comboIsValid

gameIsValid :: Game -> Boolean
gameIsValid { number: _, reveals } = all revealIsValid reveals

solve :: String -> Either String Int
solve =
  ( Str.split (Str.Pattern "\n")
      >>> Array.filter (not <<< Str.null)
      >>> traverse parseGame
      >>> -- TODO: Is there some kind of monoid or semigroup -- I can implement here for the foldl to enable a more simple sum?
        map (Array.filter gameIsValid >>> Array.foldl (\acc -> \g -> acc + g.number) 0)
  )

sum :: Array Int -> Int
sum = Array.foldl (\a -> \b -> a + b) 0

type GameSum
  = { r :: Int, g :: Int, b :: Int }

maxSum :: GameSum -> CubeCombo -> GameSum
maxSum g c = case c of
  Tuple n Red -> { r: (max n g.r), g: g.g, b: g.b }
  Tuple n Green -> { r: g.r, g: (max n g.g), b: g.b }
  Tuple n Blue -> { r: g.r, g: g.g, b: (max n g.b) }

revealSum :: GameSum -> CubeReveal -> GameSum
revealSum i = Array.foldl maxSum i

toGameSum :: Array CubeReveal -> GameSum
toGameSum = Array.foldl revealSum { r: 0, g: 0, b: 0 }

toPower :: GameSum -> Int
toPower { r, g, b } = r * g * b

gameToPower :: Game -> Int
gameToPower { number: _, reveals } = toPower $ toGameSum reveals

solve02 :: String -> Either String Int
solve02 =
  ( Str.split (Str.Pattern "\n")
      >>> Array.filter (not <<< Str.null)
      >>> traverse parseGame
      >>> map (map gameToPower >>> sum)
  )
