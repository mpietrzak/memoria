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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Memoria.Page.EditQuestionSet
    ( handleEditQuestionSet
    ) where

import Data.Default.Class (Default, def)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.Form as F
import qualified Memoria.View.EditQuestionSet as V

data EditQuestionSetFormResult =
    EditQuestionSetFormResult
        { rQuestionSetName :: Text
        }

instance Default EditQuestionSetFormResult where
    def = EditQuestionSetFormResult {rQuestionSetName = ""}

handleEditQuestionSet ::
       ( Monad m
       , C.HasAccounts m
       , C.HasCsrfToken m
       , C.HasFooterStats m
       , C.HasParams m
       , C.HasRedirects m
       , C.HasRequestMethod m
       )
    => m Text
handleEditQuestionSet = do
    accId <-
        C.getAccountId >>= \case
            Nothing -> error "Account is required"
            Just accId -> pure accId
    questionSetId <-
        C.getParam "question-set" >>= \case
            Just qsid -> pure qsid
            Nothing -> error "question-set is required"
    C.getRequestMethod >>= \case
        "GET" -> do
            csrfToken <- C.ensureCsrfToken
            dbqs <- DB.getQuestionSetByOwnerAndId accId questionSetId
            let viewData =
                    V.EditQuestionSetViewData
                        { V.evCsrfToken = csrfToken
                        , V.evQuestionSetId = questionSetId
                        , V.evQuestionSetName = DB.qsName dbqs
                        , V.evQuestionSetNameErr = Nothing
                        }
            footerStats <- C.getFooterStats
            pure $ V.renderEditQuestionSet footerStats viewData
        "POST" -> do
            csrfToken <-
                C.getParam "csrf-token" >>= \case
                    Just csrfToken -> pure csrfToken
                    Nothing -> error "csrf-token is required"
            C.checkCsrfToken csrfToken
            questionSetName <-
                C.getParam "name" >>= \case
                    Just qsname -> pure qsname
                    Nothing -> error "name is required"
            let fields =
                    [ ( "name"
                      , questionSetName
                      , \_v ->
                            case Data.Text.Lazy.strip _v of
                                "" -> Left "name is required"
                                _n
                                    | Data.Text.Lazy.length _n > 128 -> Left "too long"
                                _n -> Right _n
                      , \_vd _v -> _vd {V.evQuestionSetName = _v}
                      , \_vd _e -> _vd {V.evQuestionSetNameErr = Just _e}
                      , \_fr _v -> _fr {rQuestionSetName = _v})
                    ]
            case F.processForm fields of
                Left viewData -> do
                    footerStats <- C.getFooterStats
                    pure $ V.renderEditQuestionSet footerStats viewData
                Right result -> do
                    DB.modifyQuestionSet accId questionSetId (rQuestionSetName result)
                    C.redirect $ "question-set?id=" <> questionSetId
                    pure "redirecting to question set"
        _ -> error "Unsupported request method"
