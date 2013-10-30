{-# LANGUAGE OverloadedStrings #-}

module Handlers where

import           Prelude hiding      ( id )

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T

import           Application
import           Types.Login
import           Types.Users
import           Utils.Http
import           Utils.Rest


------------------------------------------------------------------------------
-- | Status call
getStatus :: AppHandler ()
getStatus =
    let output = "snaprest running\n\nversion: " ++ version
    in writeBS $ BS.pack output


------------------------------------------------------------------------------
-- | Examplary ping request handler
-- (may be used for simple performance tests)
pingHandler :: AppHandler ()
pingHandler = do
    count <- getIntParamDef "countparam" 10
    writeBS $ BS.pack $ replicate count '*'


------------------------------------------------------------------------------
-- | Login handler
loginHandler :: Handler App (AuthManager App) ()
loginHandler =
    jsonPut process
  where
    -- process given login information
    process login =
        either invalidLogin success
            =<< loginByUsername u p rememberToken
      where
        u = T.pack $ user login
        p = ClearText $ BS.pack $ password login

        rememberToken  = False
        success _      = jsonSimpleSuccess
        invalidLogin _ = writeErrorJson "invalid user and/or password specified"


------------------------------------------------------------------------------
-- | User logout handler
logoutHandler :: Handler App (AuthManager App) ()
logoutHandler =
    logout >> jsonSimpleSuccess


------------------------------------------------------------------------------
-- | Register new user handler
registerHandler :: Handler App (AuthManager App) ()
registerHandler =
    jsonPost process
  where
    -- process given login information
    process login
        | validU && validP = do
            exists <- usernameExists u
            if exists
                then alreadyExists
                else createUser u p >> jsonSimpleSuccess
        | otherwise = invalidLogin

      where
        u = T.pack $ user login
        p = BS.pack $ password login

        validU = length (user login) >= 4
        validP = length (password login) >= 6

    alreadyExists = writeErrorJson "username already exists"
    invalidLogin  = writeErrorJson "invalid login information specified"


------------------------------------------------------------------------------
-- | Handler to retrieve all users (paged result)
getUsersHandler :: AppHandler ()
getUsersHandler =
    getPagingResult userDb allUsersQuery mapper
  where
    mapper :: User -> User
    mapper x = x


------------------------------------------------------------------------------
-- | Handler to retrieve a specific user
getUserHandler :: AppHandler ()
getUserHandler = jsonGetId $ getUserById


------------------------------------------------------------------------------
-- | Handler to add a new user
putUserHandler :: AppHandler ()
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
deleteUserHandler :: AppHandler ()
deleteUserHandler = do
    jsonDeleteId userDb userCollection


------------------------------------------------------------------------------
-- | Handler to update a specific user
updateUserHandler :: AppHandler ()
updateUserHandler = jsonUpdateId $ \user id' -> do
    let uid = id user
    if uid == id'
        then do
            updateUser user id'
            jsonResponse user
        else writeErrorJson "IDs do not match"
