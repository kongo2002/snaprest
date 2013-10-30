{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Login where

import Control.Applicative ( (<$>), (<*>) )
import Control.Monad       ( mzero )

import Data.Aeson
import Data.Data           ( Data, Typeable )

import Utils.Template


data Login = Login
    { user     :: String
    , password :: String
    } deriving (Typeable, Data, Show, Eq)


------------------------------------------------------------------------------
-- | JSON login deserialization function
instance FromJSON Login where
    parseJSON (Object v) = Login <$>
        v .: "user" <*>
        v .: "password"
    parseJSON _ = mzero


------------------------------------------------------------------------------
-- | JSON address serialization function
instance ToJSON Login where
    toJSON = $(toJSONFunc ''Login)
