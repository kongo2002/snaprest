{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.CommDetail where

import Prelude hiding (lookup)

import Control.Applicative ((<$>), (<*>))
import Control.Monad (liftM, mzero)
import Data.Aeson hiding (String)
import Data.Bson hiding (value)
import Data.Text (Text)
import Data.Data
import Data.List (groupBy)

import Utils.Mongo
import Utils.Template

data CommType = Email | Phone | Fax
    deriving (Data, Typeable, Show, Eq, Ord)

instance Val CommType where
    val = $(toVal ''CommType)
    cast' = $(fromVal ''CommType)

instance ToJSON CommType where
    toJSON = $(toJSONFunc ''CommType)

data CommDetail = CommDetail
    {
      cdType :: CommType
    , cdValue :: String
    , cdPrim :: Bool
    } deriving (Data, Typeable, Show, Eq)

instance FromJSON CommDetail where
    parseJSON (Object v) = CommDetail
        <$> liftM parseType (v .: "type")
        <*> v .: "val"
        <*> v .:? "isPrimary" .!= False
    parseJSON _          = mzero

parseType :: Text -> CommType
parseType "email" = Email
parseType "phone" = Phone
parseType "fax"   = Fax

instance ToJSON CommDetail where
    toJSON (CommDetail t v ip) =
        object [
              "type" .= (getType t)
            , "val" .= v
            , "isPrimary" .= ip]
        where getType :: CommType -> Text
              getType Email = "email"
              getType Phone = "phone"
              getType Fax   = "fax"

instance MongoType CommDetail where
    toDoc x = [
        "type" =: cdType x,
        "val" =: cdValue x,
        "prim" =: cdPrim x]
    fromDoc x = CommDetail
        <$> lookup "type" x
        <*> lookup "val" x
        <*> lookup "prim" x

validateDetails :: [CommDetail] -> Bool
validateDetails cds =
    all isValid grouped
    where
      grouped =  groupBy (\x y -> cdType x == cdType y) cds

      isValid [] = True
      isValid xs = numPrimaries xs == 1

      numPrimaries = length . filter (\x -> cdPrim x)
