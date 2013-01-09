{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.CommDetail where

import Prelude hiding (lookup)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, mzero)
import Data.Aeson hiding (String)
import Data.Bson hiding (value)
import Data.Text
import Data.Data

import Utils.Mongo
import Utils.Template

data CommType = Email | Phone | Fax
    deriving (Data, Typeable, Show, Eq)

instance Val CommType where
    val = $(toVal ''CommType)
    cast' = $(fromVal ''CommType)

instance ToJSON CommType where
    toJSON = $(toJSONFunc ''CommType)

data CommDetail = CommDetail
    {
      typ :: CommType
    , value :: String
    } deriving (Data, Typeable, Show, Eq)

instance FromJSON CommDetail where
    parseJSON (Object v) = CommDetail
        <$> liftM parseType (v .: "type")
        <*> v .: "val"
    parseJSON _          = mzero

parseType :: Text -> CommType
parseType "email" = Email
parseType "phone" = Phone
parseType "fax"   = Fax

instance ToJSON CommDetail where
    toJSON (CommDetail t v) = object ["type" .= (getType t), "val" .= v]
        where getType :: CommType -> Text
              getType Email = "email"
              getType Phone = "phone"
              getType Fax   = "fax"

instance MongoType CommDetail where
    toDoc x = [
        "type" =: typ x,
        "val" =: value x ]
    fromDoc x = CommDetail
        <$> lookup "type" x
        <*> lookup "val" x
