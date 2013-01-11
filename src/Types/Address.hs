{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Address where

import Prelude hiding (lookup, zip)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import Data.Bson hiding (value)
import Data.Data
import Data.Maybe (catMaybes)

import Utils.Bson
import Utils.Mongo
import Utils.Template

data Address = Address
    {
      street1 :: String
    , street2 :: Maybe String
    , street3 :: Maybe String
    , zip :: Maybe String
    , city :: Maybe String
    , country :: Maybe String
    , isPrimary :: Bool
    } deriving (Typeable, Data, Show, Eq)

instance FromJSON Address where
    parseJSON (Object v) = Address <$>
        v .: "street1" <*>
        v .:? "street2" .!= Nothing <*>
        v .:? "street3" .!= Nothing <*>
        v .:? "zip" .!= Nothing <*>
        v .:? "city" .!= Nothing <*>
        v .:? "country" .!= Nothing <*>
        v .:? "isPrimary" .!= False
    parseJSON _ = mzero

instance ToJSON Address where
    toJSON = $(toJSONFunc ''Address)

instance MongoType Address where
    toDoc x = catMaybes [
        Just ("str1" =: street1 x),
        setMaybe "str2" $ street2 x,
        setMaybe "str3" $ street3 x,
        setMaybe "city" $ city x,
        setMaybe "country" $ country x,
        setMaybe "zip" $ zip x,
        Just ("prim" =: isPrimary x)]
    fromDoc x = Address
        <$> lookup "str1" x
        <*> getMaybe "str2" x
        <*> getMaybe "str3" x
        <*> getMaybe "zip" x
        <*> getMaybe "city" x
        <*> getMaybe "country" x
        <*> lookup "prim" x

