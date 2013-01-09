{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Types.Users
    (
      User(..)
    , MongoType
    , getUserByKey
    , getUserById
    , postUser
    ) where

import Prelude hiding ( id, lookup )
import Control.Applicative ( (<$>), (<*>) )
import Control.Monad.IO.Class ( MonadIO, liftIO )

import Data.Aeson
import Data.Bson
import Data.Data
import Data.Text hiding (map)
import Database.MongoDB

import Types.Address
import Types.CommDetail
import Utils.Bson
import Utils.Json
import Utils.Mongo
import Utils.Template

data User = User
    {
      id :: Int
    , firstName :: String
    , lastName :: String
    , commDetails :: [CommDetail]
    , addresses :: [Address]
    } deriving (Data, Typeable, Show, Eq)

instance FromJSON User where
    parseJSON = $(fromJSONFunc ''User)

instance ToJSON User where
    toJSON = $(toJSONFunc ''User)

instance MongoType User where
    toDoc x = [
        "id" =: id x,
        "fname" =: firstName x,
        "lname" =: lastName x,
        "cdetails" =: map toDoc (commDetails x),
        "addr" =: map toDoc (addresses x)
        ]
    fromDoc d = User
        <$> lookup "id" d
        <*> lookup "fname" d
        <*> lookup "lname" d
        <*> getList "cdetails" d
        <*> getList "addr" d

userDb :: Text
userDb = "test"

userCollection :: Text
userCollection = "Users"

getUserByKey :: MonadIO m => String -> m (Maybe User)
getUserByKey key =
    let query = select ["_id" =: (read key :: ObjectId)] userCollection
    in  mongoFindOne userDb query

getUserById :: MonadIO m => Int -> m (Maybe User)
getUserById uid =
    mongoFindOne userDb query
    where
      query = select ["id" =: uid] userCollection

postUser :: MonadIO m => User -> m String
postUser user = do
    liftIO $ putStrLn $ "Insert new user: " ++ (show user)
    liftIO $ mongoInsert userDb userCollection user
