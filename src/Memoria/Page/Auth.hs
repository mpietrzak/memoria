-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

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
