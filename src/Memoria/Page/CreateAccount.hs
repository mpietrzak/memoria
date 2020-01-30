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

module Memoria.Page.CreateAccount (
    handleCreateAccount
) where

import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (liftIO)
import Formatting ((%), fprint, text)

import Memoria.Common (HasRedirects)
import Memoria.Cookies (HasCookies(setCookie))
import Memoria.Db (HasDb)
import Memoria.Sessions (HasSessions)
import qualified Memoria.Common
import qualified Memoria.Db
import qualified Memoria.Sessions

handleCreateAccount :: (HasCookies m, HasDb m, HasRedirects m, HasSessions m) => m Text
handleCreateAccount = do
    let sessionCookieName = "session_id"
    sessionKey <- Memoria.Sessions.createSession
    liftIO $ fprint
        ("handleCreateAccount: Created new session with key: " % text % "\n")
        sessionKey
    setCookie sessionCookieName sessionKey
    accountId <- Memoria.Db.createAccount
    Memoria.Sessions.setSessionValue "account_id" accountId
    Memoria.Common.redirect "/"
    pure "account created, redirecting to index"
