{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Auth (handleAuth) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.Sessions
import qualified Memoria.Cookies

handleAuth :: (Memoria.Common.HasParams m, Memoria.Common.HasRedirects m, Memoria.Cookies.HasCookies m, Memoria.Sessions.HasSessions m) => m Text
handleAuth = do
    mToken <- Memoria.Common.getParam "token"
    case mToken of
        Nothing -> pure "No token in params"
        Just token -> do
            mAccId <- DB.getAccountIdByToken token
            case mAccId of
                Nothing -> pure "Invalid token"
                Just acc -> do
                    -- TODO: Sessions should hide cookies from us
                    sessionKey <- Memoria.Sessions.createSession
                    Memoria.Cookies.setCookie sessionCookieName sessionKey
                    Memoria.Sessions.setSessionValue "account_id" acc
                    Memoria.Common.redirect "/"
                    pure ""
    where
        sessionCookieName = "session_id"
