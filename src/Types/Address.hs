{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types.Address where

import Prelude hiding (lookup, zip)

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Data.Bson hiding (value)
import Data.Data
import GHC.Generics

import Utils.Mongo

data Address = Address
    {
      street1 :: String
    , street2 :: Maybe String
    , street3 :: Maybe String
    , zip :: Maybe String
    , city :: Maybe String
    , country :: Maybe String
    , isPrimary :: Bool
    } deriving (Typeable, Data, Show, Eq, Generic)

-- automatically derived via Generics
instance FromJSON Address

instance ToJSON Address where
    toJSON addr =
        object fields
        where
          -- FIXME
          fields = []

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
        <*> lookup "str2" x
        <*> lookup "str3" x
        <*> lookup "zip" x
        <*> lookup "city" x
        <*> lookup "country" x
        <*> lookup "prim" x

