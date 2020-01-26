{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Memoria.Page.Test (
    handleTest,
    handleTestAnswer
) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.Test as V

questionDbToView :: DB.Question -> V.Question
questionDbToView dbq = V.Question { V.qId = DB.qId dbq
                                  , V.qQuestion = DB.qQuestion dbq
                                  , V.qAnswer = DB.qAnswer dbq
                                  , V.qCreatedAt = DB.qCreatedAt dbq
                                  , V.qModifiedAt = DB.qModifiedAt dbq }

handleTest :: (Memoria.Common.HasAccounts m, Memoria.Common.HasFooterStats m, Memoria.Common.HasParams m, DB.HasDbConn m) => m Text
handleTest = do
    footerStats <- Memoria.Common.getFooterStats
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    question <- DB.getRandomQuestion accId
    let viewQuestion = questionDbToView question
    pure $ V.renderTest footerStats viewQuestion

handleTestAnswer :: (Memoria.Common.HasAccounts m, Memoria.Common.HasFooterStats m, Memoria.Common.HasParams m) => m Text
handleTestAnswer = do
    footerStats <- Memoria.Common.getFooterStats
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    questionId <- Memoria.Common.getParam "question" >>= \case
        Nothing -> error "Question is required"
        Just _q -> pure _q
    answer <- Memoria.Common.getParam "answer" >>= \case
        Nothing -> error "Answer is required"
        Just _a -> pure _a
    question <- DB.getQuestionById questionId
    let isAnswerCorrect = DB.qAnswer question == answer
    questionAnswerId <- DB.addQuestionAnswer accId questionId answer isAnswerCorrect
    let vQuestion = questionDbToView question
    pure $ V.renderTestAnswer footerStats (questionAnswerId, answer) vQuestion isAnswerCorrect
