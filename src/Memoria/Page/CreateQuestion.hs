{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.CreateQuestion (
    handleCreateQuestion
) where

import Data.Default.Class (def)
import Data.Text.Lazy (Text)

import qualified Memoria.Db
import qualified Memoria.View.CreateQuestion as V
import qualified Memoria.Common

handleCreateQuestion :: (Memoria.Common.HasAccounts m) => m Text
handleCreateQuestion = do
    accId <- Memoria.Common.getAccountId >>= \m -> case m of
        Just accId -> pure accId
        Nothing -> error "No account id"
    dbSize <- Memoria.Db.getDbSize >>= \m -> case m of
        Right s -> pure s
        Left _ -> error "Error getting db size"
    pure $ V.renderCreateQuestion dbSize def

