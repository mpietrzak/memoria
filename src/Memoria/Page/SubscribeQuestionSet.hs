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

module Memoria.Page.SubscribeQuestionSet
    ( handleSubscribeQuestionSet
    ) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.View.SubscribeQuestionSet as V

handleSubscribeQuestionSet ::
       ( Monad m
       , C.HasAccounts m
       , C.HasCsrfToken m
       , C.HasFooterStats m
       , C.HasParams m
       , C.HasRedirects m
       , C.HasRequestMethod m
       )
    => m Text
handleSubscribeQuestionSet = do
    accId <-
        C.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    questionSetId <-
        C.getParam "question-set" >>= \case
            Just qsid -> pure qsid
            Nothing -> error "question-set is required"
    method <- C.getRequestMethod
    case method of
        "GET" -> do
            sessionCsrfToken <- C.ensureCsrfToken
            footerStats <- C.getFooterStats
            let viewData =
                    V.ViewData {V.vCsrfToken = sessionCsrfToken, V.vQuestionSetId = questionSetId}
            pure $ V.renderSubscribeQuestionSet footerStats viewData
        "POST" -> do
            requestCsrfToken <-
                C.getParam "csrf-token" >>= \case
                    Just _t -> pure _t
                    Nothing -> error "csrf-token is required"
            C.checkCsrfToken requestCsrfToken
            C.getParam "decision" >>= \case
                Just "yes" -> pure ()
                _ -> error "Expected decision to be yes"
            DB.subscribeQuestionSet accId questionSetId
            C.redirect "."
            pure "ok"
        _ -> error "Unsupported request method"
