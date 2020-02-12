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

module Memoria.Page.CreateQuestionSet
    ( handleCreateQuestionSet
    ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (isNothing)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import qualified Data.UUID
import qualified Data.UUID.V4
import Formatting ((%), fprint, shown)

import Memoria.Common
    ( HasAccounts(getAccountId)
    , HasFooterStats(getFooterStats)
    , HasParams
    , HasRedirects
    , HasRequestMethod
    , getParam
    , getRequestMethod
    , redirect
    )
import Memoria.Db (HasDb)
import qualified Memoria.Db
import Memoria.View.CreateQuestionSet (renderCreateQuestionSet)
import qualified Memoria.View.CreateQuestionSet

handleCreateQuestionSet ::
       ( Monad m
       , MonadIO m
       , HasAccounts m
       , HasDb m
       , HasFooterStats m
       , HasParams m
       , HasRedirects m
       , HasRequestMethod m
       )
    => m Text
handleCreateQuestionSet = do
    footerStats <- getFooterStats
    mAccountId <- Memoria.Common.getAccountId
    method <- getRequestMethod
    formData <- getFormData method
    liftIO $
        fprint
            ("handleCreateQuestionSet: (method, isFormDataValid): (" % shown % ", " % shown % ")\n")
            method
            (isFormDataValid formData)
    case (mAccountId, method, isFormDataValid formData) of
        (Nothing, _, _) -> redirect "/" >> pure ""
        (_, "GET", _) -> pure $ renderCreateQuestionSet footerStats formData
        (_, "POST", False) -> pure $ renderCreateQuestionSet footerStats formData
        (Just accountId, "POST", True) -> createQuestionSet accountId formData
        (_, _, _) -> pure "error"
  where
    createQuestionSet accId formData = do
        id <-
            liftIO $
            Data.UUID.V4.nextRandom >>= \uuid ->
                pure $ Data.Text.Lazy.fromStrict $ Data.UUID.toText uuid
        let name = Memoria.View.CreateQuestionSet.qsfdName formData
        Memoria.Db.createQuestionSet id name accId
        redirect "/"
        pure ""
    getFormData =
        \case
            "GET" -> pure defaultFormData
            "POST" -> getRequestFormData
            _ -> pure defaultFormData
    defaultFormData =
        Memoria.View.CreateQuestionSet.CreateQuestionSetFormData
            { Memoria.View.CreateQuestionSet.qsfdName = ""
            , Memoria.View.CreateQuestionSet.qsfdNameError = Nothing
            }
    getRequestFormData = do
        mQuestionSetName <- getParam "name"
        let (name, nameErr) =
                case mQuestionSetName of
                    Nothing -> ("", Just "Name is required")
                    Just n -> (n, Nothing)
        let questionSetNameErr = Nothing
        pure $
            defaultFormData
                { Memoria.View.CreateQuestionSet.qsfdName = name
                , Memoria.View.CreateQuestionSet.qsfdNameError = nameErr
                }
    isFormDataValid d = isNothing (Memoria.View.CreateQuestionSet.qsfdNameError d)
