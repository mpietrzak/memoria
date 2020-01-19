{-# LANGUAGE LambdaCase #-}

module Memoria.Page.Test (
    handleTest
) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.Test as V

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
    where
        questionDbToView dbq = V.Question { V.qId = DB.qId dbq
                                          , V.qQuestion = DB.qQuestion dbq
                                          , V.qAnswer = DB.qAnswer dbq
                                          , V.qCreatedAt = DB.qCreatedAt dbq
                                          , V.qModifiedAt = DB.qModifiedAt dbq }
