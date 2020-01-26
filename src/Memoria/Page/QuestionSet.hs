{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Memoria.Page.QuestionSet (handleQuestionSet)
where

import Prelude hiding (id)
import Data.Text.Lazy (Text)

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.QuestionSet as V

handleQuestionSet :: (Monad m, DB.HasDb m, Memoria.Common.HasAccounts m, Memoria.Common.HasFooterStats m, Memoria.Common.HasParams m, Memoria.Common.HasRedirects m) => m Text
handleQuestionSet = do
    mAccountId <- Memoria.Common.getAccountId
    case mAccountId of
        Nothing -> Memoria.Common.redirect "/" >> pure ""
        Just accId -> do
            footerStats <- Memoria.Common.getFooterStats
            (questionSetId :: Text) <- Memoria.Common.getParam "id" >>= \case
                Nothing -> error "Missing id param"
                Just id -> pure id
            dbQuestionSet <- DB.getQuestionSet accId questionSetId
            let viewQuestionSet = dbQuestionSetToView dbQuestionSet
            dbQuestions <- DB.getQuestionSetQuestions accId questionSetId
            let viewQuestions = map dbQuestionToView dbQuestions
            pure $ V.renderQuestionSet footerStats viewQuestionSet viewQuestions
    where
        dbQuestionSetToView dbqs = V.QuestionSet { V.qsId = DB.qsId dbqs
                                                 , V.qsName = DB.qsName dbqs
                                                 , V.qsCreatedAt = DB.qsCreatedAt dbqs
                                                 , V.qsModifiedAt = DB.qsModifiedAt dbqs }
        dbQuestionToView dbq = V.Question { V.qId = DB.qId dbq
                                          , V.qQuestion = DB.qQuestion dbq
                                          , V.qAnswer = DB.qAnswer dbq
                                          , V.qScore = DB.qScore dbq
                                          , V.qCreatedAt = DB.qCreatedAt dbq
                                          , V.qModifiedAt = DB.qModifiedAt dbq }
