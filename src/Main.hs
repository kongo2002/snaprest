{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.ByteString.Char8 as BS

import           Utils.Http
import           Users

version :: String
version = "1.0"

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "snaprest - REST web services (testing project)") <|>
    route [ ("foo", writeBS "bar")
          , ("status", getStatus)
          , ("echo/:echoparam", echoHandler)
          , ("ping/:countparam", pingHandler)
          ] <|>
    dir "static" (serveDirectory ".")

getStatus :: Snap ()
getStatus =
    writeBS $ BS.pack output
    where
        output = "snaprest running\n\nversion: " ++ version

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

pingHandler :: Snap ()
pingHandler = do
    count <- getIntParamDef "countparam" 10
    writeBS $ BS.pack $ replicate count '*'

