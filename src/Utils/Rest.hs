{-# LANGUAGE OverloadedStrings #-}

module Utils.Rest where

import           Prelude hiding       ( id, elem, lookup )
import           Control.Applicative  ( (<$>), (<*>) )

import           Data.Aeson
import qualified Data.ByteString as B ( map )
import qualified Data.ByteString.Char8 as BS
import           Data.Int
import           Data.List            ( find )
import           Data.Maybe           ( fromMaybe )
import qualified Data.Map as M
import           Data.Word            ( Word8 )

import           Snap.Core

import           Utils.Http

data PagingInfo = PagingInfo
    { piPageSize :: Int
    , piPage :: Int
    } deriving (Show, Eq)

defaultPageSize :: Int
defaultPageSize = 50

maximumBodyLength :: Int64
maximumBodyLength = 100000

getSomeInt :: BS.ByteString -> (Int -> Snap ()) -> Snap ()
getSomeInt name func = method GET $ do
    idParam <- getIntParam name
    case idParam of
      Just id -> func id
      Nothing -> notFound

getSomeIntId :: (Int -> Snap ()) -> Snap ()
getSomeIntId = getSomeInt "id"

getSomeStr :: BS.ByteString -> (String -> Snap ()) -> Snap ()
getSomeStr name func = method GET $ do
    keyParam <- getParam name
    case keyParam of
      Just key -> func $ BS.unpack key
      Nothing  -> notFound

getSomeStrKey :: (String -> Snap ()) -> Snap ()
getSomeStrKey = getSomeStr "key"

getPagingParams :: Request -> Maybe PagingInfo
getPagingParams req =
    PagingInfo <$>
        getDef defaultPageSize "pagesize" <*>
        get "page"
    where
      get name = case queryParamCaseIn name req of
        Just p  -> readFirstIntMaybe p
        Nothing -> Nothing
      getDef def name = case queryParamCaseIn name req of
        Just p  -> Just $ fromMaybe def (readFirstIntMaybe p)
        Nothing -> Just $ def

queryParamCaseIn :: BS.ByteString -> Request -> Maybe [BS.ByteString]
queryParamCaseIn name rq =
    case find comp lst of
        Just (_, v) -> Just v
        Nothing     -> Nothing
    where
      lst = M.toList $ rqQueryParams rq
      comp kv = name == (B.map toLower $ fst kv)

toLower :: Word8 -> Word8
toLower w
    |  65 <= w && w <=  90 ||
      192 <= w && w <= 214 ||
      216 <= w && w <= 222 = w + 32
    | otherwise            = w

filterPaging :: PagingInfo -> [a] -> [a]
filterPaging pinfo =
    take count . drop toSkip
    where
      use = max (0 :: Int)
      count = use $ piPageSize pinfo
      toSkip = use $ count * (piPage pinfo)

getPagingResult :: ToJSON d => Snap ([d]) -> Snap ()
getPagingResult func = method GET $ do
    elements <- func
    req <- getRequest
    let filtered = case getPagingParams req of
            Just info -> filterPaging info elements
            Nothing   -> elements
    jsonResponse filtered

jsonGetId :: ToJSON d => (Int -> Snap (Maybe d)) -> Snap ()
jsonGetId func = getSomeIntId $ \id -> do
    maybeFound <- func id
    case maybeFound of
      Just elem -> jsonResponse elem
      Nothing   -> writeErrorJson $ "ID " ++ show id ++ " not found"

jsonPut :: FromJSON d => (d -> Snap ()) -> Snap ()
jsonPut func = method PUT $ do
    body <- readRequestBody maximumBodyLength
    getJson body
    where
      getJson b = case decode' b of
        Just d  -> func d
        Nothing -> writeErrorJson "invalid input given"
