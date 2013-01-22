{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Types.Users
    (
      User(..)
    , MongoType
    , getUsers
    , getUserById
    , putUser
    , validateUser
    ) where

import Prelude hiding              ( id, lookup )
import Control.Monad               ( mzero )
import Control.Applicative         ( (<$>), (<*>), Applicative )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Trans.Control ( MonadBaseControl )

import Data.Aeson
import Data.Bson hiding (value)
import Data.Char (isDigit)
import Data.Data
import Data.Text (Text)
import Database.MongoDB hiding (value)

import Types.Address
import Types.CommDetail
import Utils.Bson
import Utils.Mongo
import Utils.Template
import Utils.Validation

data User = User
    {
      id :: Int
    , firstName :: String
    , lastName :: String
    , commDetails :: [CommDetail]
    , addresses :: [Address]
    } deriving (Data, Typeable, Show, Eq)

instance FromJSON User where
    parseJSON (Object v) = User <$>
        v .:? "id" .!= 0 <*>
        v .: "firstName" <*>
        v .: "lastName" <*>
        v .:? "commDetails" .!= [] <*>
        v .:? "addresses" .!= []
    parseJSON _ = mzero

instance ToJSON User where
    toJSON = $(toJSONFunc ''User)

instance MongoType User where
    toDoc x = [
        "_id" =: id x,
        "fname" =: firstName x,
        "lname" =: lastName x,
        "cdetails" =: map toDoc (commDetails x),
        "addr" =: map toDoc (addresses x)
        ]
    fromDoc d = User
        <$> lookup "_id" d
        <*> lookup "fname" d
        <*> lookup "lname" d
        <*> getList "cdetails" d
        <*> getList "addr" d

userDb :: Text
userDb = "test"

userCollection :: Text
userCollection = "Users"

validateUser :: User -> Either String Bool
validateUser u =
    ensure "invalid firstname given" (validName $ firstName u) True >>=
    ensure "invalid lastname given" (validName $ lastName u) >>=
    ensure "invalid communication details given" (validateDetails $ commDetails u) >>=
    ensure "invalid addresses given" (validAddresses $ addresses u)

    where
      validName [] = False
      validName (x:_)
        | isDigit x = False
        | otherwise = True

      validAddresses [] = True
      validAddresses as = primaryAddresses as == 1

      primaryAddresses = length . filter (\a -> isPrimary a)

getUserById :: MonadIO m => Int -> m (Maybe User)
getUserById uid =
    mongoFindOne userDb query
    where
      query = select ["_id" =: uid] userCollection

getUsers :: MonadIO m => MonadBaseControl IO m => m [User]
getUsers =
    mongoFind userDb (select [] userCollection)

putUser :: MonadIO m => Applicative m => User -> m User
putUser u = do
    -- retrieve new user ID
    userId <- mongoGetId userDb userCollection
    let user = u { id = userId }
    liftIO $ putStrLn $ "Insert new user: " ++ (show user)

    -- insert and return new user
    liftIO $ mongoInsertIntId userDb userCollection user
