{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Settings (
    handleSettings,
    handleSettingsAddEmail
) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.Settings as V

handleSettings :: (Monad m, DB.HasDb m, Memoria.Common.HasAccounts m) => m Text
handleSettings = do
    accId <- Memoria.Common.getAccountId >>= \m -> case m of
        Just accId -> pure accId
        Nothing -> error "No account id"
    dbSize <- DB.getDbSize >>= \m -> case m of
        Right s -> pure s
        Left _ -> error "Error getting db size"
    dbAccEmails <- DB.getAccountEmails accId
    let vAccEmails = map dbToViewAccEmail dbAccEmails
    pure $ V.renderSettings dbSize vAccEmails
    where
        dbToViewAccEmail dbEmail = V.AccountEmail { V.aeId = DB.aeId dbEmail
                                                  , V.aeEmail = DB.aeEmail dbEmail
                                                  , V.aeCreatedAt = DB.aeCreatedAt dbEmail
                                                  , V.aeModifiedAt = DB.aeModifiedAt dbEmail }

handleSettingsAddEmail :: (Monad m, DB.HasDb m) => m Text
handleSettingsAddEmail = do
    dbSize <- DB.getDbSize >>= \m -> case m of
        Right s -> pure s
        Left _ -> error "Error getting db size"
    pure $ V.renderSettingsAddEmail dbSize
