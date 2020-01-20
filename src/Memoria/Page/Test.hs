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

handleTest :: (Memoria.Common.HasAccounts m, Memoria.Common.HasParams m, DB.HasDbConn m) => m Text
handleTest = do
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    question <- DB.getRandomQuestion accId
    let viewQuestion = questionDbToView question
    dbSize <- DB.getDbSize >>= \case
        Right s -> pure s
        Left _ -> error "Error getting db size"
    pure $ V.renderTest dbSize viewQuestion

handleTestAnswer :: (Memoria.Common.HasAccounts m, Memoria.Common.HasParams m) => m Text
handleTestAnswer = do
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    dbSize <- DB.getDbSize >>= \case
        Right s -> pure s
        Left _ -> error "Error getting db size"
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
    pure $ V.renderTestAnswer dbSize (questionAnswerId, answer) vQuestion isAnswerCorrect
