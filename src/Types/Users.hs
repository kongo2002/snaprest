{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Types.Users
    ( User(..)
    , userDb
    , userCollection
    , MongoType
    , allUsersQuery
    , getUsers
    , getUsersPaged
    , getUserById
    , getPrimaryEmail
    , existsUserWithEmail
    , prepareUser
    , putUser
    , updateUser
    , validateUser
    ) where

import Prelude hiding              ( id, lookup )
import Control.Monad               ( mzero )
import Control.Applicative         ( (<$>), (<*>), Applicative )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Trans.Control ( MonadBaseControl )

import Data.Aeson
import Data.Bson hiding            ( value )
import Data.Char                   ( isDigit )
import Data.Data                   ( Data, Typeable )
import Data.List                   ( find )
import Data.Maybe                  ( catMaybes )
import Data.Text                   ( Text )
import Database.MongoDB hiding     ( value, find )

import Types.Address
import Types.CommDetail
import Utils.Bson
import Utils.Mongo
import Utils.Paging
import Utils.Template
import Utils.Validation


------------------------------------------------------------------------------
-- | Record to hold user information
data User = User
    { id :: Int
    , firstName :: String
    , lastName :: String
    , commDetails :: [CommDetail]
    , addresses :: [Address]
    } deriving (Data, Typeable, Show, Eq)


------------------------------------------------------------------------------
-- | JSON deserialization function for @User@
instance FromJSON User where
    parseJSON (Object v) = User <$>
        v .:? "id" .!= 0 <*>
        v .: "firstName" <*>
        v .: "lastName" <*>
        v .:? "commDetails" .!= [] <*>
        v .:? "addresses" .!= []
    parseJSON _ = mzero


------------------------------------------------------------------------------
-- | JSON serialization function for @User@
instance ToJSON User where
    toJSON = $(toJSONFunc ''User)


------------------------------------------------------------------------------
-- | BSON conversion instance implementation of @User@
instance MongoType User where
    toDoc x = catMaybes
        [ Just ("_id" =: id x)
        , Just ("fname" =: firstName x)
        , Just ("lname" =: lastName x)
        , toList "cdetails" (commDetails x)
        , toList "addr" (addresses x) ]

    fromDoc d = User
        <$> lookup "_id" d
        <*> lookup "fname" d
        <*> lookup "lname" d
        <*> getList "cdetails" d
        <*> getList "addr" d


userDb :: Text
userDb = "test"

userCollection :: Text
userCollection = "users"


------------------------------------------------------------------------------
-- | User validation function
validateUser :: User -> Either String Bool
validateUser u =
    ensure "invalid firstname given" (validName $ firstName u) True >>=
    ensure "invalid lastname given" (validName $ lastName u) >>=
    ensure "invalid communication details given" (validateDetails $ commDetails u) >>=
    ensure "invalid addresses given" (validateAddresses $ addresses u)

  where
    validName []    = False
    validName (x:_)
        | isDigit x = False
        | otherwise = True


------------------------------------------------------------------------------
-- | Set the default communication details and addresses of the given user
prepareUser :: User -> User
prepareUser user =
    user { addresses = defAddr $ addresses user
         , commDetails = defCds $ commDetails user }
  where
    defAddr [a] = [a {isPrimary = True}]
    defAddr as  = as

    defCds [c] = [c {cdPrim = True}]
    defCds cs  = cs


------------------------------------------------------------------------------
-- | Get the primary or only email address of the @User@
getPrimaryEmail :: User -> Maybe String
getPrimaryEmail user =
    cdValue <$> getPrimary (commDetails user)
  where
    getPrimary []  = Nothing
    getPrimary [c] = Just c
    getPrimary cds = find (\c -> cdType c == Email && cdPrim c) cds


------------------------------------------------------------------------------
-- | Try to find a @User@ with the specified integer ID
getUserById :: MonadIO m => Int -> m (Maybe User)
getUserById uid =
    mongoFindOne userDb query
  where
    query = select ["_id" =: uid] userCollection


------------------------------------------------------------------------------
-- | Test if a @User@ with the specified email address already exists
existsUserWithEmail :: MonadIO m => String -> m Bool
existsUserWithEmail email =
    mongoExists userDb query
  where
    cd = ["type" =: ("email"::String), "val" =: email]
    em = ["$elemMatch" =: cd]
    query = select ["cdetails" =: em] userCollection


------------------------------------------------------------------------------
-- | Query to select all users in the database
allUsersQuery :: Query
allUsersQuery = select [] userCollection


------------------------------------------------------------------------------
-- | Get all Users stored in the database
getUsers :: MonadIO m => MonadBaseControl IO m => m [User]
getUsers = mongoFind userDb allUsersQuery


------------------------------------------------------------------------------
-- | Get all Users stored in the database (paged version)
getUsersPaged :: MonadIO m => MonadBaseControl IO m => PagingInfo
              -> m (Int, [User])
getUsersPaged = mongoFindPage userDb allUsersQuery


------------------------------------------------------------------------------
-- | Add a new @User@ to the database
putUser :: MonadIO m => Applicative m => User -> m User
putUser u = do
    -- retrieve new user ID
    userId <- mongoGetId userDb userCollection
    let user = u { id = userId }
    liftIO $ putStrLn $ "Insert new user: " ++ (show user)

    -- insert and return new user
    liftIO $ mongoInsertIntId userDb userCollection user


------------------------------------------------------------------------------
-- | Update the given @User@ by the specified ID
updateUser :: MonadIO m => User -> Int -> m ()
updateUser user id' =
    liftIO $ mongoReplaceById userDb userCollection user id'
