{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.Mongo
    ( MongoType(..)
    , mongoFind
    , mongoFindOne
    , mongoExists
    , mongoInsert
    , mongoInsertIntId
    , mongoGetId
    ) where

import Prelude hiding              ( id, elem )
import Control.Applicative
import Control.Monad               ( (>=>) )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Trans.Control ( MonadBaseControl )
import Data.Maybe                  ( catMaybes, isJust )
import Network.Socket              ( HostName )

import Database.MongoDB


------------------------------------------------------------------------------
-- | Type class to provide BSON serialization
class MongoType a where
    toDoc :: a -> Document
    fromDoc :: Document -> (Maybe a)

counterC :: Collection
counterC = "counters"

connection :: HostName
connection = "127.0.0.1"


------------------------------------------------------------------------------
-- | Execution helper function for mongo actions
exec :: MonadIO m => Database -> Action m a -> m a
exec db action = do
    pipe <- liftIO $ runIOE $ connect (host connection)
    result <- access pipe master db action
    liftIO $ close pipe
    case result of
        Right v      -> return v
        Left failure -> fail $ show failure


------------------------------------------------------------------------------
-- | Helper function to execute the mongo command `findAndModify`
findAndModify :: MonadIO m => Applicative m => Collection
              -> Document
              -> Modifier
              -> Bool
              -> Bool
              -> Action m Document
findAndModify col q u new upsert =
    let cmd = [ "findAndModify" =: col,
                "query" =: q,
                "update" =: u,
                "new" =: new,
                "upsert" =: upsert ]
    in runCommand cmd


------------------------------------------------------------------------------
-- | Helper function to execute the mongo command `findOne`
mongoFindOne :: MonadIO m => MongoType t => Database -> Query -> m (Maybe t)
mongoFindOne db query = do
    doc <- exec db $ findOne query
    return (doc >>= (fromDoc >=> Just))


------------------------------------------------------------------------------
-- | Helper function to test if a document exists for the specified query
mongoExists :: MonadIO m => Database -> Query -> m Bool
mongoExists db query = do
    doc <- exec db $ findOne query
    return $ isJust doc


------------------------------------------------------------------------------
-- | Helper function to execute the mongo command `find`
mongoFind :: MonadIO m => MonadBaseControl IO m => MongoType t => Database
          -> Query
          -> m [t]
mongoFind db query = do
    docs <- exec db $ find query >>= rest
    return (catMaybes $ map fromDoc docs)


------------------------------------------------------------------------------
-- | Helper function to retrieve a new integer ID value for the specified
-- collection
mongoGetId :: MonadIO m => Applicative m => Database -> Collection -> m Int
mongoGetId db col = do
    result <- exec db $
        findAndModify counterC
            [ "_id" =: col ]                        -- query
            [ "$inc" =: ["current" =: (1 :: Int)] ] -- update
            True                                    -- new
            True                                    -- upsert
    let v = at "value" result :: Document
    return $ (at "current" v :: Int)


------------------------------------------------------------------------------
-- | Helper function to insert a new object into the specified collection
-- and retrieve the `ObjectId` as string of the primary column
mongoInsert :: Applicative m => MonadIO m => MongoType a => Database
            -> Collection
            -> a
            -> m String
mongoInsert db col elem = do
    result <- exec db $ insert col $ toDoc elem
    case result of
      ObjId id -> return $ show id
      _        -> fail "invalid '_id' (expected ObjectId)"


------------------------------------------------------------------------------
-- | Helper function to insert a new object into the specified collection
-- and retrieve the integer primary column value
mongoInsertIntId :: Applicative m => MonadIO m => MongoType a => Database
                 -> Collection
                 -> a
                 -> m a
mongoInsertIntId db col elem = do
    result <- exec db $ insert col $ toDoc elem
    case result of
      Int32 _ -> return elem
      _        -> fail "invalid '_id' (expected Integer)"
