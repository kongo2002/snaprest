{-# LANGUAGE TemplateHaskell #-}

module Application where

import Control.Lens         ( makeLenses )

import Snap.Snaplet
import Snap.Snaplet.Auth    ( AuthManager )
import Snap.Snaplet.Session ( SessionManager )


version :: String
version = "0.1"


------------------------------------------------------------------------------
-- | Main application state
data App = App
    { _auth :: Snaplet (AuthManager App)
    , _sess :: Snaplet SessionManager }


makeLenses ''App


------------------------------------------------------------------------------
-- | Simple type abbreviation for application handlers
type AppHandler = Handler App App
