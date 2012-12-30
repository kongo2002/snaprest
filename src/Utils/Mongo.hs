module Utils.Mongo where

import Control.Monad ((>=>))
import Control.Monad.IO.Class (liftIO)

import Database.MongoDB

class MongoType a where
    toDoc :: a -> Document
    fromDoc :: Document -> (Maybe a)

exec db action = do
    pipe <- liftIO $ runIOE $ connect (host "127.0.0.1")
    result <- access pipe master db action
    liftIO $ close pipe
    case result of
        Right value  -> return value
        Left failure -> fail $ show failure

mongoFindOne db query = do
    doc <- exec db $ findOne query
    return (doc >>= (fromDoc >=> Just))
