module Utils.Rest where

import Prelude hiding (id)
import Data.Data
import Data.Aeson.Generic as JSON
import Snap.Core

import Utils.Http

getSomeById :: (Int -> Snap ()) -> Snap ()
getSomeById func = method GET $ do
    idParam <- getIntParam "id"
    case idParam of
      Just id -> func id
      Nothing -> notFound

jsonGet :: Data d => (Int -> Snap (Maybe d)) -> Snap ()
jsonGet func = getSomeById $ \id -> do
    maybeFound <- func id
    case maybeFound of
      Just elem -> writeLBS $ JSON.encode $ elem
      Nothing   -> notFound
