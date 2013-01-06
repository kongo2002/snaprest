{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Address where

import Prelude hiding (lookup, zip)

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Aeson.TH (mkParseJSON)
import Data.Bson hiding (value)
import Data.Data

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
    parseJSON = $(mkParseJSON id ''Address)

instance ToJSON Address where
    toJSON = $(recordToJSON ''Address)

instance MongoType Address where
    toDoc x = [
        "str1" =: street1 x,
        "str2" =: street2 x,
        "str3" =: street3 x,
        "city" =: city x,
        "country" =: country x,
        "prim" =: isPrimary x,
        "zip" =: zip x ]
    fromDoc x = Address
        <$> lookup "str1" x
        <*> getMaybe "str2" x
        <*> getMaybe "str3" x
        <*> getMaybe "zip" x
        <*> getMaybe "city" x
        <*> getMaybe "country" x
        <*> lookup "prim" x

