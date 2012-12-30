{-# LANGUAGE OverloadedStrings #-}

module Utils.Rest where

import           Prelude hiding (id, elem)
import           Data.Data
import           Data.Maybe (fromJust)
import qualified Data.Aeson.Generic as JSON
import qualified Data.ByteString.Char8 as BS
import           Snap.Core

import           Utils.Http

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

jsonGetId :: Data d => (Int -> Snap (Maybe d)) -> Snap ()
jsonGetId func = getSomeIntId $ \id -> do
    maybeFound <- func id
    case maybeFound of
      Just elem -> do
        modifyResponse $ setToJson
        writeLBS $ JSON.encode $ elem
      Nothing   -> notFound

readBodyJson :: MonadSnap m => Data d => m d
readBodyJson = readRequestBody maximumBodyLength >>= return . fromJust . JSON.decode'
