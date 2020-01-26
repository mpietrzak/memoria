{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Settings (
    handleSettings,
    handleSettingsAddEmail
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (Default(def))
import Data.Text.Lazy (Text)
import Formatting ((%), fprint, text, shown)
import qualified Data.Text.Lazy

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.Settings as V

validateEmail val = case val of
    Nothing -> Left "Email is required"
    Just _v -> case Data.Text.Lazy.strip _v of
        "" -> Left "Email is required"
        _v -> case Data.Text.Lazy.find (== '@') _v of
            Nothing -> Left "Email must have a @ in it"
            Just _ -> Right (Just _v)

processField fieldName validator fieldSetter errSetter formData = do
    mval <- Memoria.Common.getParam fieldName
    liftIO $ fprint
        ("processField: Field " % text % " has value " % shown % "\n")
        fieldName
        mval
    let validationResult = validator mval
    case validationResult of
        Left err -> pure (False, fieldSetter mval $ errSetter err formData)
        Right val -> pure (True, fieldSetter val formData)

handleSettings :: (Monad m, DB.HasDb m, Memoria.Common.HasAccounts m, Memoria.Common.HasFooterStats m) => m Text
handleSettings = do
    footerStats <- Memoria.Common.getFooterStats
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    dbAccEmails <- DB.getAccountEmails accId
    let vAccEmails = map dbToViewAccEmail dbAccEmails
    pure $ V.renderSettings footerStats vAccEmails
    where
        dbToViewAccEmail dbEmail = V.AccountEmail { V.aeId = DB.aeId dbEmail
                                                  , V.aeEmail = DB.aeEmail dbEmail
                                                  , V.aeCreatedAt = DB.aeCreatedAt dbEmail
                                                  , V.aeModifiedAt = DB.aeModifiedAt dbEmail }

handleSettingsAddEmail :: (Monad m, DB.HasDb m, Memoria.Common.HasAccounts m, Memoria.Common.HasFooterStats m, Memoria.Common.HasParams m, Memoria.Common.HasRedirects m, Memoria.Common.HasRequestMethod m) => m Text
handleSettingsAddEmail = do
    footerStats <- Memoria.Common.getFooterStats
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    let fields = [
                (
                    "email",
                    validateEmail,
                    \v f -> f { V.aefEmail = v },
                    \e f -> f { V.aefEmailErr = Just e }
                )
             ]
    method <- Memoria.Common.getRequestMethod
    case method of
        "GET" -> pure $ V.renderSettingsAddEmail footerStats def
        "POST" -> do
            (isFormValid, formData) <- processFormData fields
            liftIO $ fprint
                ("handleSettingsAddEmail: formData: " % shown % "\n")
                formData
            if isFormValid then
               (case V.aefEmail formData of
                    Nothing -> error "Email missing in valid form data"
                    Just e -> do
                        DB.addEmail accId e
                        Memoria.Common.redirect "settings"
                        pure "")
            else
                pure $ V.renderSettingsAddEmail footerStats formData
        _ -> error "Unsupported method"
    where
        processFormData = processFormDataGo True def
        processFormDataGo isFormValid formData fields = case fields of
            (f:fs) -> do
                let (name, validator, fieldSetter, errSetter) = f
                (isFieldValid, newFormData) <- processField
                    name
                    validator
                    fieldSetter
                    errSetter
                    formData
                let newIsFormValid = isFieldValid && isFormValid
                processFormDataGo newIsFormValid newFormData fs
            _ -> pure (isFormValid, formData)

