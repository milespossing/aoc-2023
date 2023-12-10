module Day04 where

import Prelude
import Utils
import Data.Traversable (traverse)
import Effect (Effect)
import Data.Foldable (sum)
import Data.Tuple (Tuple(..))
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Array.NonEmpty (NonEmptyArray(..))
import Data.String as Str
import Data.String.Regex (Regex(..), regex, match)
import Data.String.Regex.Flags (noFlags)
import Data.Int (pow, fromString)

type Side
  = Array Int

type Card
  = Tuple Side Side

matchRegex :: Regex -> String -> Maybe (NonEmptyArray (Maybe String))
matchRegex regexp = match regexp

parseAsTuple :: Array (Maybe String) -> Maybe (Tuple String String)
parseAsTuple [ Just _, Just s1, Just s2 ] = Just (Tuple s1 s2)

parseAsTuple _ = Nothing

toIntArray :: String -> Either String (Array Int)
toIntArray s = do
  let
    entries = Array.filter (not <<< Str.null) $ Str.split (Str.Pattern " ") s
  note "failed to cast to int" $ traverse fromString entries

parseCard :: Regex -> String -> Either String Card
parseCard r s = do
  match <- note "no matches found" $ matchRegex r s
  Tuple winners guesses <- note ("could not parse as tuple: " <> show match) $ parseAsTuple $ NEA.toArray match
  winnerArr <- toIntArray winners
  guessArr <- toIntArray guesses
  pure (Tuple winnerArr guessArr)

getCards :: Regex -> String -> Either String (Array Card)
getCards regexp file =
  let
    lines = getLines file
  in
    traverse (parseCard regexp) lines

scoreCard :: Card -> Int
scoreCard (Tuple winners guesses) =
  let
    sortedWinners = Array.sort winners

    sortedGuesses = Array.sort guesses
  in
    aux sortedWinners sortedGuesses 0
  where
  aux :: Array Int -> Array Int -> Int -> Int
  aux [] _ acc = acc

  aux _ [] acc = acc

  aux a b acc = case Tuple (Array.uncons a) (Array.uncons b) of
    (Tuple Nothing _) -> acc
    (Tuple _ Nothing) -> acc
    (Tuple (Just { head: head_a, tail: tail_a }) (Just { head: head_b, tail: tail_b }))
      | head_a == head_b -> aux tail_a tail_b (acc + 1)
    (Tuple (Just { head: head_a, tail: tail_a }) (Just { head: head_b, tail: tail_b }))
      | head_a > head_b -> aux a tail_b acc
    (Tuple (Just { head: head_a, tail: tail_a }) (Just { head: head_b, tail: tail_b })) -> aux tail_a b acc

scoreCard1 :: Int -> Int
scoreCard1 0 = 0

scoreCard1 i = pow 2 (i - 1)

solve1 :: String -> Either String Int
solve1 file = do
  regexp <- regex "Card +\\d+: ([\\d ]+) \\| ([\\d ]+)" noFlags
  cards <- getCards regexp file
  let
    scores = map scoreCard1 $ map scoreCard cards
  Right (sum scores)

-- scoreCards2 :: Array Card -> Int
-- scoreCards2 cards = aux cards Array.replicate (Array.length cards) 1 acc where
--   aux :: Array Card -> Array Int -> Int -> Int
--   aux c s acc = case Tuple (Array.uncons c) (Array.uncons s) of
--       Tuple Nothing _ -> acc
--       Tuple _ Nothing -> acc
--       Tuple (Just { head: card, tail: rest }) (Just { head: copies, tail: rest_copies })
solve2 :: String -> Either String Int
solve2 file = do
  regexp <- regex "Card +\\d+: ([\\d ]+) \\| ([\\d ]+)" noFlags
  cards <- getCards regexp file
  Left "Not implemented"
