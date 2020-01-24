{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Memoria.Page.QuestionSet (handleQuestionSet)
where

import Prelude hiding (id)
import Data.Text.Lazy (Text)

import Memoria.View.QuestionSet (QuestionSet(..), renderQuestionSet)
import qualified Memoria.Common
import qualified Memoria.Db
import qualified Memoria.Db as DB
import qualified Memoria.View.QuestionSet as V

handleQuestionSet :: (Monad m, Memoria.Common.HasAccounts m, Memoria.Db.HasDb m, Memoria.Common.HasParams m, Memoria.Common.HasRedirects m) => m Text
handleQuestionSet = do
    mAccountId <- Memoria.Common.getAccountId
    case mAccountId of
        Nothing -> Memoria.Common.redirect "/" >> pure ""
        Just accId -> do
            dbSize <- Memoria.Db.getDbSize >>= \case
                Right s -> pure s
                Left err -> error "error getting db size"
            (questionSetId :: Text) <- Memoria.Common.getParam "id" >>= \case
                Nothing -> error "Missing id param"
                Just id -> pure id
            dbQuestionSet <- Memoria.Db.getQuestionSet accId questionSetId
            let viewQuestionSet = dbQuestionSetToView dbQuestionSet
            dbQuestions <- Memoria.Db.getQuestionSetQuestions accId questionSetId
            let viewQuestions = map dbQuestionToView dbQuestions

            pure $ renderQuestionSet dbSize viewQuestionSet viewQuestions
    where
        dbQuestionSetToView dbqs = QuestionSet { id = Memoria.Db.id dbqs
                                               , name = Memoria.Db.name dbqs }
        dbQuestionToView dbq = V.Question { V.qId = DB.qId dbq
                                          , V.qQuestion = DB.qQuestion dbq
                                          , V.qAnswer = DB.qAnswer dbq
                                          , V.qCreatedAt = DB.qCreatedAt dbq
                                          , V.qModifiedAt = DB.qModifiedAt dbq }
