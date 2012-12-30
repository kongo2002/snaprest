{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Users
    (
      User(..)
    , MongoType
    , getUserByKey
    , getUserById
    , postUser
    ) where

import Prelude hiding (id, lookup)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Bson
import Data.Data
import Data.Text
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

userDb :: Text
userDb = "test"

userCollection :: Text
userCollection = "Users"

getUserByKey :: MonadIO m => String -> m (Maybe User)
getUserByKey key =
    let query = select ["_id" =: (read key :: ObjectId)] userCollection
    in  mongoFindOne userDb query

getUserById :: MonadIO m => Int -> m (Maybe User)
getUserById id =
    mongoFindOne userDb query
    where
      query = select ["id" =: id] userCollection

postUser :: MonadIO m => User -> m String
postUser user = do
    liftIO $ putStrLn $ "Insert new user: " ++ (show user)
    liftIO $ mongoInsert userDb userCollection user


