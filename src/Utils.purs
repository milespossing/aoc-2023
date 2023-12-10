module Utils where

import Prelude
import Data.Array as Array
import Data.String as Str

getLines :: String -> Array String
getLines = Array.filter (not <<< Str.null) <<< Str.split (Str.Pattern "\n")
