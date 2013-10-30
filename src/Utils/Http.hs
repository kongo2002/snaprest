{-# LANGUAGE OverloadedStrings #-}

module Utils.Http where

import           Snap.Core
import           Data.Aeson                 ( ToJSON, toJSON, object, (.=)
                                            , encode )
import           Data.Maybe                 ( fromMaybe, mapMaybe )

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.ByteString.Lex.Double ( readDouble )

import           Application


------------------------------------------------------------------------------
-- | Record to represent error information
data ErrorJson = ErrorJson
    {
      success :: Bool
    , message :: String
    , errorCode :: Maybe Int
    } deriving (Show, Eq)


------------------------------------------------------------------------------
-- | ToJSON instance implementation for ErrorJson
instance ToJSON ErrorJson where
    toJSON (ErrorJson s m (Just c)) =
        object ["success" .= s, "message" .= m, "errorCode" .= c]
    toJSON (ErrorJson s m Nothing) =
        object ["success" .= s, "message" .= m]


------------------------------------------------------------------------------
-- | Try to parse an integer from the given bytestring
readIntMaybe :: BS.ByteString -> Maybe Int
readIntMaybe bs = do
    p <- BS.readInt bs
    return $ fst p


------------------------------------------------------------------------------
-- | Try to parse a double from the given bytestring
readDoubleMaybe :: BS.ByteString -> Maybe Double
readDoubleMaybe bs = do
    p <- readDouble bs
    return $ fst p


------------------------------------------------------------------------------
-- | Try to parse the first found integer from the given bytestring.
-- This function may be used to parse query parameters.
readFirstIntMaybe :: [BS.ByteString] -> Maybe Int
readFirstIntMaybe []    = Nothing
readFirstIntMaybe (x:_) = readIntMaybe x


------------------------------------------------------------------------------
-- | Try to parse the first found double from the given bytestring.
-- This function may be used to parse query parameters.
readFirstDoubleMaybe :: [BS.ByteString] -> Maybe Double
readFirstDoubleMaybe []    = Nothing
readFirstDoubleMaybe (x:_) = readDoubleMaybe x


------------------------------------------------------------------------------
-- | Retrieve an integer parameter from the given request.
rqIntParam :: BS.ByteString -> Request -> Maybe Int
rqIntParam bs rq = do
    p <- rqParam bs rq
    readFirstIntMaybe p


------------------------------------------------------------------------------
-- | Retrieve an integer parameter from the given request or return a
-- default value.
rqIntParamDef :: Int -> BS.ByteString -> Request -> Int
rqIntParamDef d bs rq =
    fromMaybe d (rqIntParam bs rq)


------------------------------------------------------------------------------
-- | Retrieve a double parameter from the given request.
rqDoubleParam :: BS.ByteString -> Request -> Maybe Double
rqDoubleParam bs rq = do
    p <- rqParam bs rq
    readFirstDoubleMaybe p


------------------------------------------------------------------------------
-- | Retrieve a double parameter from the given request or return a
-- default value.
rqDoubleParamDef :: Double -> BS.ByteString -> Request -> Double
rqDoubleParamDef d bs rq =
    fromMaybe d (rqDoubleParam bs rq)


------------------------------------------------------------------------------
-- | Retrieve a specific integer parameter.
getIntParam :: MonadSnap m => BS.ByteString -> m (Maybe Int)
getIntParam bs = do
    r <- getRequest
    return $ rqIntParam bs r


------------------------------------------------------------------------------
-- | Retrieve a specific integer parameter or a default value.
getIntParamDef :: MonadSnap m => BS.ByteString -> Int -> m (Int)
getIntParamDef bs d = do
    r <- getRequest
    return $ rqIntParamDef d bs r


------------------------------------------------------------------------------
-- | Retrieve a comma separated parameter list.
rqListParam :: BS.ByteString -> Request -> [BS.ByteString]
rqListParam bs rq =
    maybe [] splitParams (rqParam bs rq)
  where
    splitParams = concatMap (BS.split ',')


------------------------------------------------------------------------------
-- | Retrieve a comma separated integer parameter list.
rqIntListParam :: BS.ByteString -> Request -> [Int]
rqIntListParam bs rq =
    mapMaybe readIntMaybe (rqListParam bs rq)


------------------------------------------------------------------------------
-- | Set the response's content type to 'application/json'
setToJson :: Response -> Response
setToJson = setContentType $ BS.pack "application/json; charset=utf-8"


------------------------------------------------------------------------------
-- | Return a simple success:true JSON response
jsonSimpleSuccess :: MonadSnap m => m ()
jsonSimpleSuccess = do
    modifyResponse setToJson
    writeLBS "{\"success\":true}"


------------------------------------------------------------------------------
-- | Return a JSON response wrapped in a '{data:...,success:true}' form.
jsonResponse :: ToJSON a => MonadSnap m => a -> m ()
jsonResponse element = do
    modifyResponse setToJson
    let elemJson = encode element
    writeLBS $ "{\"data\":" `LBS.append` elemJson `LBS.append` ",\"success\":true}"


------------------------------------------------------------------------------
-- | Write an error response with a given code and message.
writeErrorResponse :: MonadSnap m => Int -> String -> m ()
writeErrorResponse code msg = do
    modifyResponse $ setResponseCode code
    writeBS $ BS.pack msg


------------------------------------------------------------------------------
-- | Write a JSON formatted error response with an optional error code
-- and message.
writeErrorJsonCode :: MonadSnap m => Maybe Int -> String -> m ()
writeErrorJsonCode code msg = do
    modifyResponse setToJson
    writeLBS $ encode $ ErrorJson False msg code


------------------------------------------------------------------------------
-- | Write a JSON formatted error response with a given message.
writeErrorJson :: MonadSnap m => String -> m ()
writeErrorJson = writeErrorJsonCode Nothing


------------------------------------------------------------------------------
-- | Return a 404 error response with a specified message.
notFoundMsg :: MonadSnap m => String -> m ()
notFoundMsg msg = writeErrorResponse 404 msg


------------------------------------------------------------------------------
-- | Return a 404 error response.
notFound :: MonadSnap m => m ()
notFound = notFoundMsg "Not found"


------------------------------------------------------------------------------
-- | Return a 500 error response with a specified message.
invalidMsg :: MonadSnap m => String -> m ()
invalidMsg = writeErrorResponse 500


------------------------------------------------------------------------------
-- | Return a 500 error response.
invalidInput :: MonadSnap m => m ()
invalidInput = invalidMsg "Invalid input given"
