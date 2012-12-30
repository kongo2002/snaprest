{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Users where

import Prelude hiding (lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO)

import Data.Bson
import Data.Data
import Database.MongoDB

import Utils.Mongo

data User = User {
    userId :: Int,
    fname :: String,
    lname :: String } deriving (Data, Typeable, Show, Eq)

instance MongoType User where
    toDoc x = [
        "id" =: userId x,
        "fname" =: fname x,
        "lname" =: lname x ]
    fromDoc d = User
        <$> lookup "id" d
        <*> lookup "fname" d
        <*> lookup "lname" d

userDb = "test"
userCollection = "Users"

getUserByKey :: (MonadIO m) => String -> m (Maybe User)
getUserByKey key =
    let query = select ["_id" =: (read key :: ObjectId)] userCollection
    in  mongoFindOne userDb query

getUserById :: (MonadIO m) => Int -> m (Maybe User)
getUserById id =
    mongoFindOne userDb query
    where
      query = select ["id" =: id] userCollection
