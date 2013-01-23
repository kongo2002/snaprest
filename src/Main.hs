{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative    ( (<|>) )
import           Control.Monad.IO.Class ( MonadIO, liftIO )

import           Snap.Core
import           Snap.Util.FileServe
import           Snap.Http.Server

import qualified Data.ByteString.Char8 as BS

import           Types.Users
import           Utils.Http
import           Utils.Rest

version :: String
version = "0.1"

main :: IO ()
main = quickHttpServe site


------------------------------------------------------------------------------
-- | Site request dispatcher
site :: Snap ()
site =
    ifTop (writeBS "snaprest - REST web services (testing project)") <|>
    route [ ("status",           getStatus)
          , ("ping/:countparam", pingHandler)
          , ("users/user/:id",   getUserHandler)
          , ("users/user",       putUserHandler)
          , ("users",            getUsersHandler)
          ] <|>
    dir "static" (serveDirectory ".")


------------------------------------------------------------------------------
-- | Status call
getStatus :: Snap ()
getStatus =
    let output = "snaprest running\n\nversion: " ++ version
    in writeBS $ BS.pack output


------------------------------------------------------------------------------
-- | Examplary ping request handler
-- (may be used for simple performance tests)
pingHandler :: Snap ()
pingHandler = do
    count <- getIntParamDef "countparam" 10
    writeBS $ BS.pack $ replicate count '*'


------------------------------------------------------------------------------
-- | Handler to retrieve all users (paged result)
getUsersHandler :: Snap()
getUsersHandler = getPagingResult $ liftIO getUsers


------------------------------------------------------------------------------
-- | Handler to retrieve a specific user
getUserHandler :: Snap ()
getUserHandler = jsonGetId $ getUserById


------------------------------------------------------------------------------
-- | Handler to add a new new user
putUserHandler :: Snap ()
putUserHandler =
    jsonPut $ \user ->
        case validateUser user of
            Left err -> writeErrorJson err
            Right _  -> do
                u <- putUser user
                jsonResponse u
