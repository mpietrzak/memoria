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

module Memoria.Page.QuestionAnswers (
    handleQuestionAnswers
) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.QuestionAnswers as V

answerDbToView :: DB.Answer -> V.Answer
answerDbToView ans = V.Answer { V.ansId = DB.ansId ans
                              , V.ansAnswer = DB.ansAnswer ans
                              , V.ansAnsweredAt = DB.ansAnsweredAt ans
                              , V.ansIsCorrect = DB.ansIsCorrect ans }

handleQuestionAnswers :: (Memoria.Common.HasAccounts m, Memoria.Common.HasFooterStats m, Memoria.Common.HasParams m, DB.HasDbConn m) => m Text
handleQuestionAnswers = do
    footerStats <- Memoria.Common.getFooterStats
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    questionId <- Memoria.Common.getParam "question" >>= \case
        Nothing -> error "Question id is required"
        Just _q -> pure _q
    answers <- DB.getAnswers accId questionId
    pure $ V.renderQuestionAnswers footerStats $ map answerDbToView answers

