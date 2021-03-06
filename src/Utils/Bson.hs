module Utils.Bson
    ( getList
    , toList
    , getMaybe
    , setMaybe
    ) where

import Prelude hiding ( lookup, elem )
import Data.Bson      ( Document, Label, Field, Val, lookup, (=:) )
import Data.Maybe     ( mapMaybe )

import Utils.Mongo    ( MongoType, fromDoc, toDoc )


------------------------------------------------------------------------------
-- | Helper function to return an empty list intead of Nothing if the
-- requested field is not found
getList :: MongoType a => Label -> Document -> Maybe [a]
getList k doc =
    Just $ maybe [] (mapMaybe fromDoc) (lookup k doc)


------------------------------------------------------------------------------
-- | Helper function to return @Nothing@ in case of an empty list
toList :: MongoType a => Label -> [a] -> Maybe Field
toList _ [] = Nothing
toList l xs = Just (l =: map toDoc xs)


------------------------------------------------------------------------------
-- | Helper function to return @Just Nothing@ in case the requested field
-- is not found
getMaybe :: Val a => Label -> Document -> Maybe (Maybe a)
getMaybe k doc = Just $ lookup k doc


------------------------------------------------------------------------------
-- | Helper function to return a @Just@ field based on a given @Maybe@ value
setMaybe :: Val v => Label -> Maybe v -> Maybe Field
setMaybe k (Just x) = Just (k =: x)
setMaybe _ Nothing  = Nothing
