module Utils.Bson
    (
      getList
    ) where

import Prelude hiding (lookup)
import Data.Bson (Document, Label, look, lookup)
import Data.Maybe (mapMaybe)

import Utils.Mongo

getList :: MongoType a => Label -> Document -> Maybe [a]
getList k doc =
    Just list
    where
      elem = lookup k doc
      list = case elem of
          Just something -> mapMaybe fromDoc something
          Nothing        -> []
