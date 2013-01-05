module Utils.Json where

import Data.Aeson (object)
import Data.Aeson.Types
import Data.Maybe (catMaybes)
import Data.Text (Text)

selectPairs :: [Maybe Pair] -> Value
selectPairs = object . catMaybes

nonEmpty :: ToJSON a => [a] -> Text -> Maybe Pair
nonEmpty [] _    = Nothing
nonEmpty xs name = Just $ name .= xs

nonEmpty2 :: ToJSON a => [a] -> Text -> [Pair]
nonEmpty2 [] _    = []
nonEmpty2 xs name = [name .= xs]
