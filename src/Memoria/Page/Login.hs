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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Login
    ( handleLogin
    ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.List
import qualified Data.Map.Lazy
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import Formatting ((%), fprint, int, text)
import qualified Network.Mail.Mime
import qualified Network.Mail.SMTP
import qualified Network.Mail.SMTP.Types
import qualified Network.Socket

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.Login as V

handleLogin ::
       ( Monad m
       , MonadIO m
       , DB.HasDb m
       , Memoria.Common.HasFooterStats m
       , Memoria.Common.HasParams m
       , Memoria.Common.HasRequestMethod m
       )
    => m Text
handleLogin = do
    footerStats <- Memoria.Common.getFooterStats
    method <- Memoria.Common.getRequestMethod
    case method of
        "GET" -> pure $ V.renderLogin footerStats
        "POST" -> do
            mEmail <- Memoria.Common.getParam "email"
            case mEmail of
                Nothing -> error "Email is required"
                Just email -> do
                    accounts <- DB.getAccountsByEmail email
                    liftIO $ fprint ("We have " % int % " accounts\n") (Data.List.length accounts)
                    when (not (Data.List.null accounts)) $ do
                        accountLoginTokens <- addLoginTokens Data.Map.Lazy.empty accounts
                        let emailText = makeEmailText accounts accountLoginTokens
                        liftIO $ fprint ("Auth email to send:\n" % text % "\n") emailText
                        liftIO $ Network.Mail.SMTP.sendMail "localhost" (makeEmail email emailText)
                    pure "Check your inbox"
  where
    addLoginTokens loginTokenMap accounts =
        case accounts of
            [] -> pure loginTokenMap
            acc:rest -> do
                let accId = DB.aId acc
                token <- DB.addLoginToken accId
                addLoginTokens (Data.Map.Lazy.insert accId token loginTokenMap) rest
    makeEmail to body =
        Network.Mail.SMTP.simpleMail
            (Network.Mail.SMTP.Types.Address
                 Nothing
                 (Data.Text.Lazy.toStrict "memoria@typesafe.org"))
            [Network.Mail.SMTP.Types.Address Nothing (Data.Text.Lazy.toStrict to)]
            []
            []
            "Memoria login"
            [Network.Mail.Mime.plainPart body]
    makeEmailText accounts tokens =
        "Click a link to login:\n" <>
        "\n" <>
        Data.Text.Lazy.concat (map ((<> "\n") . makeLoginLink tokens) accounts) <>
        "\n" <> "-- \n" <> "memoria\n"
    makeLoginLink tokens acc = "https://memoria.typesafe.org/auth?token=" <> token
      where
        accId = DB.aId acc
        token =
            case Data.Map.Lazy.lookup accId tokens of
                Just _t -> _t
                Nothing -> error "No token for account"
