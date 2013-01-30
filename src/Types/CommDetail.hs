{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.CommDetail where

import Prelude hiding      ( lookup )

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad       ( liftM, mzero )

import Data.Aeson hiding   ( String )
import Data.Bson hiding    ( value )
import Data.Data           ( Data, Typeable )
import Data.List           ( groupBy )
import Data.Text           ( Text )

import Utils.Mongo
import Utils.Template


------------------------------------------------------------------------------
-- | Communication types
data CommType = Email | Phone | Fax
    deriving (Data, Typeable, Show, Eq, Ord)


------------------------------------------------------------------------------
-- | Value implementation of @CommType@
instance Val CommType where
    val = $(toVal ''CommType)
    cast' = $(fromVal ''CommType)


------------------------------------------------------------------------------
-- | @CommType@ JSON serialization function
instance ToJSON CommType where
    toJSON = $(toJSONFunc ''CommType)


------------------------------------------------------------------------------
-- | Communication detail record type
data CommDetail = CommDetail
    { cdType :: CommType
    , cdValue :: String
    , cdPrim :: Bool
    } deriving (Data, Typeable, Show, Eq)


------------------------------------------------------------------------------
-- | Communication detail JSON deserialization implementation
instance FromJSON CommDetail where
    parseJSON (Object v) = CommDetail
        <$> liftM parseType (v .: "type")
        <*> v .: "value"
        <*> v .:? "isPrimary" .!= False
    parseJSON _          = mzero

parseType :: Text -> CommType
parseType "email" = Email
parseType "phone" = Phone
parseType "fax"   = Fax
parseType ct      = error $ "Invalid communication type given: " ++ (show ct)


------------------------------------------------------------------------------
-- | Communication detail JSON serialization implementation
instance ToJSON CommDetail where
    toJSON (CommDetail t v ip) = object
        [ "type" .= (getType t)
        , "value" .= v
        , "isPrimary" .= ip ]
      where getType :: CommType -> Text
            getType Email = "email"
            getType Phone = "phone"
            getType Fax   = "fax"


------------------------------------------------------------------------------
-- | Communication detail mongo conversion implementation
instance MongoType CommDetail where
    toDoc x =
        [ "type" =: cdType x
        , "val" =: cdValue x
        , "prim" =: cdPrim x ]
    fromDoc x = CommDetail
        <$> lookup "type" x
        <*> lookup "val" x
        <*> lookup "prim" x


------------------------------------------------------------------------------
-- | Validate the given communication details
validateDetails :: [CommDetail] -> Bool
validateDetails cds =
    all isValid grouped
  where
    grouped =  groupBy (\x y -> cdType x == cdType y) cds

    isValid [] = True
    isValid xs = numPrimaries xs == 1

    numPrimaries = length . filter (\x -> cdPrim x)
