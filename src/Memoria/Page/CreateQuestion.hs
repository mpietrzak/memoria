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

module Memoria.Page.CreateQuestion
    ( handleCreateQuestion
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import qualified Data.Maybe
import Data.Text.Lazy (Text)
import Formatting ((%), fprint, shown, text)

import qualified Memoria.Common
import qualified Memoria.Db
import qualified Memoria.View.CreateQuestion as V

validateRequired fieldName val =
    case val of
        Nothing -> Left $ fieldName <> " is required"
        Just "" -> Left $ fieldName <> " is required"
        Just t -> Right (Just t)

validateQuestion :: Maybe Text -> Either Text (Maybe Text)
validateQuestion = validateRequired "Question"

validateAnswer = validateRequired "Answer"

processField fieldName validator fieldSetter errSetter formData = do
    mval <- Memoria.Common.getParam fieldName
    liftIO $ fprint ("processField: Field " % text % " has value " % shown % "\n") fieldName mval
    let validationResult = validator mval
    case validationResult of
        Left err -> pure (False, errSetter err formData)
        Right val -> pure (True, fieldSetter val formData)

handleCreateQuestion ::
       ( Memoria.Common.HasAccounts m
       , Memoria.Common.HasCsrfToken m
       , Memoria.Common.HasFooterStats m
       , Memoria.Common.HasParams m
       , Memoria.Common.HasRedirects m
       , Memoria.Common.HasRequestMethod m
       )
    => m Text
handleCreateQuestion = do
    footerStats <- Memoria.Common.getFooterStats
    accId <-
        Memoria.Common.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    questionSetId <-
        Memoria.Common.getParam "question-set" >>= \case
            Just id -> pure id
            Nothing -> error "Question set id is required"
    let fields =
            [ ( "question"
              , validateQuestion
              , \v f -> f {V.question = v}
              , \e f -> f {V.questionErr = Just e})
            , ("answer", validateAnswer, \v f -> f {V.answer = v}, \e f -> f {V.answerErr = Just e})
            ]
    csrfToken <- Memoria.Common.ensureCsrfToken
    method <- Memoria.Common.getRequestMethod
    (isFormDataValid, formData) <-
        case method of
            "GET" -> pure (False, def)
            _ -> processFormData fields
    case (method, isFormDataValid) of
        ("GET", _) -> pure $ V.renderCreateQuestion footerStats questionSetId csrfToken formData
        ("POST", False) ->
            pure $ V.renderCreateQuestion footerStats questionSetId csrfToken formData
        ("POST", True) -> do
            actualCsrfToken <-
                Memoria.Common.getParam "csrf-token" >>= \case
                    Nothing -> error "CSRF token is required"
                    Just t -> pure t
            Memoria.Common.checkCsrfToken actualCsrfToken
            Memoria.Db.addQuestion
                accId
                questionSetId
                (Data.Maybe.fromJust $ V.question formData, Data.Maybe.fromJust $ V.answer formData)
            Memoria.Common.redirect $ "question-set?id=" <> questionSetId
            pure ""
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
