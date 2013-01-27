{-# LANGUAGE OverloadedStrings #-}

module Utils.Rest where

import           Prelude hiding       ( id, elem, lookup )
import           Control.Applicative  ( (<$>), (<*>) )
import           Control.Monad        ( liftM )

import           Data.Aeson
import qualified Data.ByteString as B ( map )
import qualified Data.ByteString.Char8 as BS
import           Data.Int
import           Data.List            ( find )
import           Data.Maybe           ( fromMaybe )
import qualified Data.Map as M
import           Data.Word            ( Word8 )
import           Database.MongoDB     ( Database, Collection )

import           Snap.Core

import           Utils.Http
import           Utils.Paging
import           Utils.Mongo


maximumBodyLength :: Int64
maximumBodyLength = 100000


------------------------------------------------------------------------------
-- | Execute the given function based on a specified identifier integer
getSomeInt :: BS.ByteString -> (Int -> Snap ()) -> Snap ()
getSomeInt name func = method GET $ do
    idParam <- getIntParam name
    case idParam of
      Just id -> func id
      Nothing -> notFound


------------------------------------------------------------------------------
-- | Execute the given function based on a 'id' integer identifier
getSomeIntId :: (Int -> Snap ()) -> Snap ()
getSomeIntId = getSomeInt "id"


------------------------------------------------------------------------------
-- | Execute the given function based on a specified identifier string
getSomeStr :: BS.ByteString -> (String -> Snap ()) -> Snap ()
getSomeStr name func = method GET $ do
    keyParam <- getParam name
    case keyParam of
      Just key -> func $ BS.unpack key
      Nothing  -> notFound


------------------------------------------------------------------------------
-- | Execute the given function based on a 'key' string identifier
getSomeStrKey :: (String -> Snap ()) -> Snap ()
getSomeStrKey = getSomeStr "key"


------------------------------------------------------------------------------
-- | Try to retrieve PagingInfo from the given Request
getPagingParams :: Request -> Maybe PagingInfo
getPagingParams req =
    PagingInfo <$>
        getDef defaultPageSize "pagesize" <*>
        get "page"
    where
      toMax = liftM (max 1)
      get name = case queryParamCI name req of
        Just p  -> toMax $ readFirstIntMaybe p
        Nothing -> Nothing
      getDef def name = case queryParamCI name req of
        Just p  -> Just $ fromMaybe def (toMax $ readFirstIntMaybe p)
        Nothing -> Just $ def


------------------------------------------------------------------------------
-- | Retrieve a list of query parameters with a specified name while
-- matching case insensitive
queryParamCI :: BS.ByteString -> Request -> Maybe [BS.ByteString]
queryParamCI name rq =
    case find comp lst of
        Just (_, v) -> Just v
        Nothing     -> Nothing
    where
      lst = M.toList $ rqQueryParams rq
      comp kv = name == (B.map toLower $ fst kv)


------------------------------------------------------------------------------
-- | Lowercase convert the given bytestring word
toLower :: Word8 -> Word8
toLower w
    |  65 <= w && w <=  90 ||
      192 <= w && w <= 214 ||
      216 <= w && w <= 222 = w + 32
    | otherwise            = w


------------------------------------------------------------------------------
-- | Process the result of the given function with optional PagingResult
-- information
getPagingResult :: ToJSON d => (PagingInfo -> Snap (Int, [d]))
                -> Snap [d]
                -> Snap ()
getPagingResult pageFunc func = method GET $ do
    req <- getRequest
    case getPagingParams req of
        Just info -> do
            (c, elements) <- pageFunc info
            -- TODO: build full URI
            let base = rqContextPath req
            let link = buildLinkHeader info c base
            modifyResponse $ setHeader "Link" link
            jsonResponse elements
        Nothing   -> do
            elements <- func
            jsonResponse elements


------------------------------------------------------------------------------
-- | Helper function to process a GET request for a specified ID
jsonGetId :: ToJSON d => (Int -> Snap (Maybe d)) -> Snap ()
jsonGetId func = getSomeIntId $ \id -> do
    maybeFound <- func id
    case maybeFound of
      Just elem -> jsonResponse elem
      Nothing   -> writeErrorJson $ "ID " ++ show id ++ " not found"


------------------------------------------------------------------------------
-- | Helper function to process a DELETE request for a specified ID
jsonDeleteId :: Database -> Collection -> Snap ()
jsonDeleteId db col = method DELETE $ do
    id <- getIntParam "id"
    case id of
        Just identifier -> do
            mongoRemoveById db col identifier
            modifyResponse setToJson
            writeLBS "{\"data\":true,\"success\":true}"
        Nothing ->
            writeErrorJson "invalid ID given"


------------------------------------------------------------------------------
-- | Helper function to process a PUT request
jsonPut :: FromJSON d => (d -> Snap ()) -> Snap ()
jsonPut func = method PUT $ do
    body <- readRequestBody maximumBodyLength
    getJson body
    where
      getJson b = case decode' b of
        Just d  -> func d
        Nothing -> writeErrorJson "invalid input given"
