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
{-# LANGUAGE LambdaCase #-}

module Memoria.Page.Test
    ( handleTest
    , handleTestAnswer
    ) where

import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.Test as V

questionDbToView :: DB.Question -> V.Question
questionDbToView dbq =
    V.Question
        { V.qId = DB.qId dbq
        , V.qQuestion = DB.qQuestion dbq
        , V.qAnswer = DB.qAnswer dbq
        , V.qScore = DB.qScore dbq
        , V.qCreatedAt = DB.qCreatedAt dbq
        , V.qModifiedAt = DB.qModifiedAt dbq
        }

handleTest ::
       ( Memoria.Common.HasAccounts m
       , Memoria.Common.HasFooterStats m
       , Memoria.Common.HasParams m
       , DB.HasDbConn m
       )
    => m Text
handleTest = do
    footerStats <- Memoria.Common.getFooterStats
    accId <-
        Memoria.Common.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    question <- DB.getRandomQuestion accId
    let viewQuestion = questionDbToView question
    pure $ V.renderTest footerStats viewQuestion

handleTestAnswer ::
       (Memoria.Common.HasAccounts m, Memoria.Common.HasFooterStats m, Memoria.Common.HasParams m)
    => m Text
handleTestAnswer = do
    footerStats <- Memoria.Common.getFooterStats
    accId <-
        Memoria.Common.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    questionId <-
        Memoria.Common.getParam "question" >>= \case
            Nothing -> error "Question is required"
            Just _q -> pure _q
    answer <-
        Memoria.Common.getParam "answer" >>= \case
            Nothing -> error "Answer is required"
            Just _a -> pure $ Data.Text.Lazy.strip _a
    question <- DB.getQuestionById questionId
    let isAnswerCorrect = Data.Text.Lazy.strip (DB.qAnswer question) == answer
    questionAnswerId <- DB.addQuestionAnswer accId questionId answer isAnswerCorrect
    let vQuestion = questionDbToView question
    pure $ V.renderTestAnswer footerStats (questionAnswerId, answer) vQuestion isAnswerCorrect
