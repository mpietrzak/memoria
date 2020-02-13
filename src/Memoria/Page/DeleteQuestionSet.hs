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

module Memoria.Page.DeleteQuestionSet
    ( handleDeleteQuestionSet
    ) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.View.DeleteQuestionSet as V

handleDeleteQuestionSet ::
       ( DB.HasDb m
       , C.HasAccounts m
       , C.HasRedirects m
       , C.HasRequestMethod m
       , C.HasFooterStats m
       , C.HasParams m
       )
    => m Text
handleDeleteQuestionSet = do
    accId <-
        C.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    questionSetId <-
        C.getParam "question-set" >>= \case
            Just id -> pure id
            Nothing -> error "Question set id is required"
    method <- C.getRequestMethod
    case method of
        "GET" -> do
            footerStats <- C.getFooterStats
            dbQuestionSet <- DB.getQuestionSetByOwnerAndId accId questionSetId
            let vQuestionSet =
                    V.QuestionSet
                        { V.qsId = DB.qsId dbQuestionSet
                        , V.qsName = DB.qsName dbQuestionSet
                        , V.qsCreatedAt = DB.qsCreatedAt dbQuestionSet
                        , V.qsModifiedAt = DB.qsModifiedAt dbQuestionSet
                        }
            pure $ V.renderDeleteQuestionSet footerStats vQuestionSet
        "POST" -> do
            DB.setQuestionSetDeleted accId questionSetId
            C.redirect "/"
            pure ""
        _ -> error "Unsupported HTTP method"
