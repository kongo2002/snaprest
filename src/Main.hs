{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<|>))

import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.ByteString.Char8 as BS

import           Types.Users
import           Utils.Http
import           Utils.Rest

version :: String
version = "1.0"

main :: IO ()
main = quickHttpServe site

site :: Snap ()
site =
    ifTop (writeBS "snaprest - REST web services (testing project)") <|>
    route [ ("status",           getStatus)
          , ("ping/:countparam", pingHandler)
          , ("users/user/:id",   getUserHandler)
          , ("users/user",       putUserHandler)
          ] <|>
    dir "static" (serveDirectory ".")

getStatus :: Snap ()
getStatus =
    let output = "snaprest running\n\nversion: " ++ version
    in writeBS $ BS.pack output

pingHandler :: Snap ()
pingHandler = do
    count <- getIntParamDef "countparam" 10
    writeBS $ BS.pack $ replicate count '*'

getUserHandler :: Snap ()
getUserHandler = jsonGetId $ getUserById

putUserHandler :: Snap ()
putUserHandler =
    jsonPut $ \user ->
        case validateUser user of
            Left err -> writeErrorJson err
            Right _  -> do
                u <- putUser user
                jsonResponse u
