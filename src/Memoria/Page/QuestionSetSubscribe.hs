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

module Memoria.Page.QuestionSetSubscribe
    ( handleQuestionSetSubscribe
    ) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.View.QuestionSetSubscribe as V

handleQuestionSetSubscribe :: (C.HasAccounts m, C.HasFooterStats m, C.HasParams m) => m Text
handleQuestionSetSubscribe = do
    footerStats <- C.getFooterStats
    questionSetId <-
        C.getParam "question-set" >>= \case
            Nothing -> error "Question set id is required"
            Just _qsid -> pure _qsid
    dbQuestionSet <- DB.getQuestionSetById questionSetId
    let vQuestionSet = dbToViewQuestionSet dbQuestionSet
    pure $ V.renderQuestionSetSubscribe footerStats vQuestionSet
  where
    dbToViewQuestionSet dbqs =
        V.QuestionSet
            { V.qsId = DB.qsId dbqs
            , V.qsName = DB.qsName dbqs
            , V.qsOwner = DB.qsOwner dbqs
            , V.qsCreatedAt = DB.qsCreatedAt dbqs
            , V.qsModifiedAt = DB.qsModifiedAt dbqs
            }
