module Utils.Bson
    (
      getList
    , getMaybe
    , setMaybe
    ) where

import Prelude hiding ( lookup, elem )
import Data.Bson      ( Document, Label, Field, Val, lookup, (=:) )
import Data.Maybe     ( mapMaybe )

import Utils.Mongo (MongoType, fromDoc)

getList :: MongoType a => Label -> Document -> Maybe [a]
getList k doc =
    Just $ maybe [] (mapMaybe fromDoc) (lookup k doc)

getMaybe :: Val a => Label -> Document -> Maybe (Maybe a)
getMaybe k doc = Just $ lookup k doc

setMaybe :: Val v => Label -> Maybe v -> Maybe Field
setMaybe k (Just x) = Just (k =: x)
setMaybe _ Nothing  = Nothing
