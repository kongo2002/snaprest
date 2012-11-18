{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.ByteString.Char8 as BS

import           Data.Maybe (fromMaybe, mapMaybe)
{- import           Data.ByteString.Lex.Double (readDouble) -}

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "snaprest - REST web services (testing project)") <|>
    route [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("ping/:countparam", pingHandler)
          ] <|>
    dir "static" (serveDirectory ".")

readIntMaybe :: BS.ByteString -> Maybe Int
readIntMaybe bs = do
    p <- BS.readInt bs
    return $ fst p

readFirstIntMaybe :: [BS.ByteString] -> Maybe Int
readFirstIntMaybe []    = Nothing
readFirstIntMaybe (x:_) = readIntMaybe x

rqIntParam :: BS.ByteString -> Request -> Maybe Int
rqIntParam bs rq = do
    p <- rqParam bs rq
    readFirstIntMaybe p

rqIntParamDef :: Int -> BS.ByteString -> Request -> Int
rqIntParamDef d bs rq =
    fromMaybe d (rqIntParam bs rq)

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

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

pingHandler :: Snap ()
pingHandler = do
    count <- getParam "countparam"
    maybe (writeBS "invalid no or invalid count given")
        (writeBS . BS.pack . (flip replicate '*') . (getCount 10)) count
    where
        getCount d bs = case BS.readInt bs of
            Just (v, _) -> v
            Nothing     -> d

