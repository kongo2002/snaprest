{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative ((<|>))
import           Control.Monad.IO.Class (liftIO)

import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.ByteString.Char8 as BS

import           Users
import           Utils.Http
import           Utils.Rest

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
          , ("users/user/:id", getUserHandler)
          , ("users/user", postUserHandler)
          ] <|>
    dir "static" (serveDirectory ".")

getStatus :: Snap ()
getStatus =
    let output = "snaprest running\n\nversion: " ++ version
    in writeBS $ BS.pack output

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param

pingHandler :: Snap ()
pingHandler = do
    count <- getIntParamDef "countparam" 10
    writeBS $ BS.pack $ replicate count '*'

getUserHandler :: Snap ()
getUserHandler = jsonGetId $ getUserById

postUserHandler :: Snap ()
postUserHandler =
    jsonPost $ handleUser
    where
      handleUser u =
        liftIO $ putStrLn $ "New user " ++ (show (u :: User))
