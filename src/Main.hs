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

import           Application
import           Handlers
import           Utils.Http             ( notFoundJson )


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
        initCookieSessionManager "key.txt" "cookie" Nothing
    -- init auth
    a <- nestSnaplet "auth" auth $
        initJsonFileAuthManager defAuthSettings sess "login.json"
    -- routes
    addRoutes [ ("", ifTop             root)
              -- user calls
              , ("users/user/:id",     getUserHandler)
              , ("users/user/:id",     deleteUserHandler)
              , ("users/user/:id",     updateUserHandler)
              , ("users/user",         putUserHandler)
              , ("users",              getUsersHandler)
              -- authorization calls
              , ("auth/login",  with auth $ loginHandler)
              , ("auth/logout", with auth $ logoutHandler)
              , ("auth/new",    with auth $ registerHandler)
              -- various calls
              , ("status",             getStatus)
              , ("ping/:countparam",   pingHandler)
              ]

    wrapSite (<|> dir "static" (serveDirectory "."))
    wrapSite (<|> notFoundJson)

    return $ App a s
  where
    root = writeBS "snaprest - REST web services (testing project)"


