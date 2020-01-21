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

handleQuestionAnswers :: (Memoria.Common.HasAccounts m, Memoria.Common.HasParams m, DB.HasDbConn m) => m Text
handleQuestionAnswers = do
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    dbSize <- DB.getDbSize >>= \case
        Right s -> pure s
        Left _ -> error "Error getting db size"
    questionId <- Memoria.Common.getParam "question" >>= \case
        Nothing -> error "Question id is required"
        Just _q -> pure _q
    answers <- DB.getAnswers accId questionId
    pure $ V.renderQuestionAnswers dbSize $ map answerDbToView answers

