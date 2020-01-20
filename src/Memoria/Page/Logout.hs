{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Logout (handleLogout) where

import Data.Text.Lazy (Text)

import qualified Memoria.Sessions
import qualified Memoria.Common

handleLogout :: (Memoria.Common.HasRedirects m, Memoria.Sessions.HasSessions m) => m Text
handleLogout = do
    Memoria.Sessions.deleteSessionValue Memoria.Sessions.sessionAccountIdName
    Memoria.Common.redirect "/"
    pure "Redirecting..."
