{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Types.CommDetail where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Data.Bson hiding (value)
import Data.Data
import Utils.Mongo

data CommType = Email | Phone | Fax
    deriving (Data, Typeable, Show, Eq)

instance Val CommType where
    val Email = String "email"
    val Phone = String "phone"
    val Fax   = String "fax"

    cast' (String "email") = Just Email
    cast' (String "phone") = Just Phone
    cast' (String "fax")   = Just Fax
    cast' _                = Nothing

data CommDetail = CommDetail
    {
      typ :: CommType
    , value :: String
    } deriving (Data, Typeable, Show, Eq)

instance MongoType CommDetail where
    toDoc x = [
        "type" =: typ x,
        "val" =: value x ]
    fromDoc x = CommDetail
        <$> lookup "type" x
        <*> lookup "val" x
