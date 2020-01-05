{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Memoria.Page.QuestionSet (handleQuestionSet)
where

import Prelude hiding (id)
import Data.Text.Lazy (Text)

import qualified Memoria.Db
import qualified Memoria.Common
import Memoria.View.QuestionSet (renderQuestionSet)
import Memoria.View.QuestionSet (QuestionSet(..))

handleQuestionSet :: (Monad m, Memoria.Common.HasAccounts m, Memoria.Db.HasDb m, Memoria.Common.HasParams m, Memoria.Common.HasRedirects m) => m Text
handleQuestionSet = do
    mAccountId <- Memoria.Common.getAccountId
    case mAccountId of
        Nothing -> Memoria.Common.redirect "/" >> pure ""
        Just accId -> do
            dbSize <- Memoria.Db.getDbSize >>= \ms -> case ms of
                Right s -> pure s
                Left err -> error "error getting db size"

            (questionSetId :: Text) <- Memoria.Common.getParam "id" >>= \m -> case m of
                Nothing -> error "Missing id param"
                Just id -> pure id

            dbQuestionSet <- Memoria.Db.getQuestionSet accId questionSetId
            let viewQuestionSet = dbQuestionSetToView dbQuestionSet

            pure $ renderQuestionSet dbSize viewQuestionSet
    where
        dbQuestionSetToView dbqs = QuestionSet { id = Memoria.Db.id dbqs
                                               , name = Memoria.Db.name dbqs }
