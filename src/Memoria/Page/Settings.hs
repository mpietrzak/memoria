-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Settings
    ( handleSetNickname
    , handleSettings
    , handleSettingsAddEmail
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (Default(def))
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import Formatting ((%), fprint, shown, text)

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.Form as F
import qualified Memoria.View.Settings as V

data NicknameFormResult =
    NicknameFormResult
        { rNickname :: Text
        }

instance Default NicknameFormResult where
    def = NicknameFormResult {rNickname = ""}

validateEmail :: Maybe Text -> Either Text (Maybe Text)
validateEmail val =
    case val of
        Nothing -> Left "Email is required"
        Just _v ->
            case Data.Text.Lazy.strip _v of
                "" -> Left "Email is required"
                _v ->
                    case Data.Text.Lazy.find (== '@') _v of
                        Nothing -> Left "Email must have a @ in it"
                        Just _ -> Right (Just _v)

processField fieldName validator fieldSetter errSetter formData = do
    mval <- C.getParam fieldName
    liftIO $ fprint ("processField: Field " % text % " has value " % shown % "\n") fieldName mval
    let validationResult = validator mval
    case validationResult of
        Left err -> pure (False, fieldSetter mval $ errSetter err formData)
        Right val -> pure (True, fieldSetter val formData)

handleSetNickname ::
       (C.HasAccounts m, C.HasFooterStats m, C.HasParams m, C.HasRedirects m, C.HasRequestMethod m)
    => m Text
handleSetNickname = do
    accId <-
        C.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    method <- C.getRequestMethod
    case method of
        "GET" -> do
            mNickname <- DB.getAccountNickname accId
            footerStats <- C.getFooterStats
            let nicknameViewData =
                    V.NicknameViewData {V.nvNickname = mNickname, V.nvNicknameErr = Nothing}
            pure $ V.renderSetNickname footerStats nicknameViewData
        "POST" -> do
            nickname <-
                C.getParam "nickname" >>= \case
                    Just _n -> pure _n
                    Nothing -> error "nickname is required"
            let fields =
                    [ ( "nickname"
                      , nickname
                      , \_n ->
                            case Data.Text.Lazy.strip _n of
                                "" -> Left "nickname is required"
                                _ -> Right $ _n
                      -- view data expects Maybe Text; HTTP request always carries Text
                      , \_vd _fv -> _vd {V.nvNickname = Just _fv}
                      , \_vd _err -> _vd {V.nvNicknameErr = Just _err}
                      , \_res _fv -> _res {rNickname = _fv})
                    ]
            case F.processForm fields of
                Left viewData -> do
                    footerStats <- C.getFooterStats
                    pure $ V.renderSetNickname footerStats viewData
                Right formResult -> do
                    DB.setNickname accId (rNickname formResult)
                    C.redirect "settings"
                    pure "redirecting to settings..."
        _ -> error "Invalid HTTP method"

handleSettings :: (Monad m, DB.HasDb m, C.HasAccounts m, C.HasFooterStats m) => m Text
handleSettings = do
    footerStats <- C.getFooterStats
    accId <-
        C.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    dbAccEmails <- DB.getAccountEmails accId
    let vAccEmails = map dbToViewAccEmail dbAccEmails
    nickname <- DB.getAccountNickname accId
    pure $ V.renderSettings footerStats nickname vAccEmails
  where
    dbToViewAccEmail dbEmail =
        V.AccountEmail
            { V.aeId = DB.aeId dbEmail
            , V.aeEmail = DB.aeEmail dbEmail
            , V.aeCreatedAt = DB.aeCreatedAt dbEmail
            , V.aeModifiedAt = DB.aeModifiedAt dbEmail
            }

handleSettingsAddEmail ::
       ( Monad m
       , DB.HasDb m
       , C.HasAccounts m
       , C.HasFooterStats m
       , C.HasParams m
       , C.HasRedirects m
       , C.HasRequestMethod m
       )
    => m Text
handleSettingsAddEmail = do
    footerStats <- C.getFooterStats
    accId <-
        C.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    let fields =
            [ ( "email"
              , validateEmail
              , \v f -> f {V.aefEmail = v}
              , \e f -> f {V.aefEmailErr = Just e})
            ]
    method <- C.getRequestMethod
    case method of
        "GET" -> pure $ V.renderSettingsAddEmail footerStats def
        "POST" -> do
            (isFormValid, formData) <- processFormData fields
            liftIO $ fprint ("handleSettingsAddEmail: formData: " % shown % "\n") formData
            if isFormValid
                then (case V.aefEmail formData of
                          Nothing -> error "Email missing in valid form data"
                          Just e -> do
                              DB.addEmail accId e
                              C.redirect "settings"
                              pure "")
                else pure $ V.renderSettingsAddEmail footerStats formData
        _ -> error "Unsupported method"
  where
    processFormData = processFormDataGo True def
    processFormDataGo isFormValid formData fields =
        case fields of
            (f:fs) -> do
                let (name, validator, fieldSetter, errSetter) = f
                (isFieldValid, newFormData) <-
                    processField name validator fieldSetter errSetter formData
                let newIsFormValid = isFieldValid && isFormValid
                processFormDataGo newIsFormValid newFormData fs
            _ -> pure (isFormValid, formData)
