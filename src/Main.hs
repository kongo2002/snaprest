{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude hiding         ( id )
import           Control.Applicative    ( (<|>) )

import           Snap.Core
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe

import qualified Data.ByteString.Char8 as BS

import           Types.Application
import           Types.Users
import           Utils.Http
import           Utils.Rest


version :: String
version = "0.1"


------------------------------------------------------------------------------
-- | Main application
main :: IO ()
main = serveSnaplet defaultConfig initApp


------------------------------------------------------------------------------
-- | Main snaplet initialization
initApp :: SnapletInit App App
initApp = makeSnaplet "snaprest" "REST web services" Nothing $ do
    -- init sessions
    s <- nestSnaplet "session" sess $
        initCookieSessionManager "skey.txt" "cookie" Nothing
    -- init auth
    a <- nestSnaplet "auth" auth $
        initJsonFileAuthManager defAuthSettings sess "login.json"
    -- routes
    addRoutes [ ("", ifTop           root)
              , ("status",           getStatus)
              , ("ping/:countparam", pingHandler)
              , ("users/user/:id",   getUserHandler)
              , ("users/user/:id",   deleteUserHandler)
              , ("users/user/:id",   updateUserHandler)
              , ("users/user",       putUserHandler)
              , ("users",            getUsersHandler)
              ]
    wrapSite (<|> dir "static" (serveDirectory "."))
    return $ App a s
  where
    root = writeBS "snaprest - REST web services (testing project)"


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
