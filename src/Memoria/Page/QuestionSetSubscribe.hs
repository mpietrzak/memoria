{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.QuestionSetSubscribe (
    handleQuestionSetSubscribe
) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.View.QuestionSetSubscribe as V

handleQuestionSetSubscribe :: (C.HasAccounts m, C.HasFooterStats m, C.HasParams m) => m Text
handleQuestionSetSubscribe = do
    footerStats <- C.getFooterStats
    questionSetId <- C.getParam "question-set" >>= \case
        Nothing -> error "Question set id is required"
        Just _qsid -> pure _qsid
    dbQuestionSet <- DB.getQuestionSetById questionSetId
    let vQuestionSet = dbToViewQuestionSet dbQuestionSet
    pure $ V.renderQuestionSetSubscribe footerStats vQuestionSet
    where
        dbToViewQuestionSet dbqs = V.QuestionSet { V.qsId = DB.qsId dbqs
                                                 , V.qsName = DB.qsName dbqs
                                                 , V.qsOwner = DB.qsOwner dbqs
                                                 , V.qsCreatedAt = DB.qsCreatedAt dbqs
                                                 , V.qsModifiedAt = DB.qsModifiedAt dbqs }
