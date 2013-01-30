{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude hiding         ( id )
import           Control.Applicative    ( (<|>) )

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
          , ("users/user/:id",   deleteUserHandler)
          , ("users/user/:id",   updateUserHandler)
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
getUsersHandler :: Snap ()
getUsersHandler =
    getPagingResult userDb allUsersQuery mapper
    where
      mapper :: User -> User
      mapper x = x


------------------------------------------------------------------------------
-- | Handler to retrieve a specific user
getUserHandler :: Snap ()
getUserHandler = jsonGetId $ getUserById


------------------------------------------------------------------------------
-- | Handler to add a new user
putUserHandler :: Snap ()
putUserHandler =
    jsonPut $ \user ->
        let user' = prepareUser user
        in case validateUser user' of
            Left err -> writeErrorJson err
            Right _  -> do
                exists <- emailExists
                case exists of
                    (False, _) -> do
                        u <- putUser user'
                        jsonResponse u
                    (True, email) ->
                        writeErrorJson $ "User with email '" ++ email ++ "' already exists"
                where
                  emailExists = do
                    case getPrimaryEmail user' of
                      Just email -> do
                        ex <- existsUserWithEmail email
                        return (ex, email)
                      Nothing -> return (False, [])


------------------------------------------------------------------------------
-- | Handler to remove a specific user by ID
deleteUserHandler :: Snap ()
deleteUserHandler = do
    jsonDeleteId userDb userCollection


------------------------------------------------------------------------------
-- | Handler to update a specific user
updateUserHandler :: Snap ()
updateUserHandler = jsonUpdateId $ \user id' -> do
    let userId = id user
    if userId == id'
       then do
         updateUser user id'
         jsonResponse user
       else writeErrorJson "IDs do not match"
