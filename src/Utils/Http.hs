module Utils.Http where

import           Snap.Core
import           Data.Maybe (fromMaybe, mapMaybe)

import qualified Data.ByteString.Char8 as BS

import           Data.ByteString.Lex.Double (readDouble)

readIntMaybe :: BS.ByteString -> Maybe Int
readIntMaybe bs = do
    p <- BS.readInt bs
    return $ fst p

readDoubleMaybe :: BS.ByteString -> Maybe Double
readDoubleMaybe bs = do
    p <- readDouble bs
    return $ fst p

readFirstIntMaybe :: [BS.ByteString] -> Maybe Int
readFirstIntMaybe []    = Nothing
readFirstIntMaybe (x:_) = readIntMaybe x

readFirstDoubleMaybe :: [BS.ByteString] -> Maybe Double
readFirstDoubleMaybe []    = Nothing
readFirstDoubleMaybe (x:_) = readDoubleMaybe x

rqIntParam :: BS.ByteString -> Request -> Maybe Int
rqIntParam bs rq = do
    p <- rqParam bs rq
    readFirstIntMaybe p

rqIntParamDef :: Int -> BS.ByteString -> Request -> Int
rqIntParamDef d bs rq =
    fromMaybe d (rqIntParam bs rq)

rqDoubleParam :: BS.ByteString -> Request -> Maybe Double
rqDoubleParam bs rq = do
    p <- rqParam bs rq
    readFirstDoubleMaybe p

rqDoubleParamDef :: Double -> BS.ByteString -> Request -> Double
rqDoubleParamDef d bs rq =
    fromMaybe d (rqDoubleParam bs rq)

getIntParam :: MonadSnap m => BS.ByteString -> m (Maybe Int)
getIntParam bs = do
    r <- getRequest
    return $ rqIntParam bs r

getIntParamDef :: MonadSnap m => BS.ByteString -> Int -> m (Int)
getIntParamDef bs d = do
    r <- getRequest
    return $ rqIntParamDef d bs r

rqListParam :: BS.ByteString -> Request -> [BS.ByteString]
rqListParam bs rq =
    maybe [] splitParams (rqParam bs rq)
    where
        splitParams = concatMap (BS.split ',')

rqIntListParam :: BS.ByteString -> Request -> [Int]
rqIntListParam bs rq =
    mapMaybe readIntMaybe (rqListParam bs rq)
