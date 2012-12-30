module Utils.Mongo
    (
      MongoType(..)
    , mongoFindOne
    ) where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (MonadIO, liftIO)

import Database.MongoDB

class MongoType a where
    toDoc :: a -> Document
    fromDoc :: Document -> (Maybe a)

exec :: MonadIO m => Database -> Action m a -> m a
exec db action = do
    pipe <- liftIO $ runIOE $ connect (host "127.0.0.1")
    result <- access pipe master db action
    liftIO $ close pipe
    case result of
        Right v      -> return v
        Left failure -> fail $ show failure

mongoFindOne :: MonadIO m => MongoType t => Database -> Query -> m (Maybe t)
mongoFindOne db query = do
    doc <- exec db $ findOne query
    return (doc >>= (fromDoc >=> Just))
