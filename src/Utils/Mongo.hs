{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Utils.Mongo
    (
      MongoType(..)
    , mongoFind
    , mongoFindOne
    , mongoInsert
    , mongoInsertIntId
    , mongoGetId
    ) where

import Prelude hiding              ( id, elem )
import Control.Applicative
import Control.Monad               ( (>=>) )
import Control.Monad.IO.Class      ( MonadIO, liftIO )
import Control.Monad.Trans.Control ( MonadBaseControl )
import Data.Maybe                  ( catMaybes )

import Database.MongoDB

class MongoType a where
    toDoc :: a -> Document
    fromDoc :: Document -> (Maybe a)

counterC :: Collection
counterC = "counters"

exec :: MonadIO m => Database -> Action m a -> m a
exec db action = do
    pipe <- liftIO $ runIOE $ connect (host "127.0.0.1")
    result <- access pipe master db action
    liftIO $ close pipe
    case result of
        Right v      -> return v
        Left failure -> fail $ show failure

findAndModify :: MonadIO m => Applicative m => Collection -> Document -> Modifier -> Bool -> Bool -> Action m Document
findAndModify col q u new upsert =
    let cmd = [ "findAndModify" =: col,
                "query" =: q,
                "update" =: u,
                "new" =: new,
                "upsert" =: upsert ]
    in runCommand cmd

mongoFindOne :: MonadIO m => MongoType t => Database -> Query -> m (Maybe t)
mongoFindOne db query = do
    doc <- exec db $ findOne query
    return (doc >>= (fromDoc >=> Just))

mongoFind :: MonadIO m => MonadBaseControl IO m => MongoType t => Database -> Query -> m [t]
mongoFind db query = do
    docs <- exec db $ find query >>= rest
    return (catMaybes $ map fromDoc docs)

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

mongoInsert :: Applicative m => MonadIO m => MongoType a => Database -> Collection -> a -> m String
mongoInsert db col elem = do
    result <- exec db $ insert col $ toDoc elem
    case result of
      ObjId id -> return $ show id
      _        -> fail "invalid '_id' (expected ObjectId)"

mongoInsertIntId :: Applicative m => MonadIO m => MongoType a => Database -> Collection -> a -> m a
mongoInsertIntId db col elem = do
    result <- exec db $ insert col $ toDoc elem
    case result of
      Int32 _ -> return elem
      _        -> fail "invalid '_id' (expected Integer)"
