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
{-# LANGUAGE ScopedTypeVariables #-}

module Memoria.Page.EditAnswer
    ( handleEditAnswer
    ) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Default.Class (Default, def)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import Formatting ((%), fprint, shown)

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.Form
import qualified Memoria.Form
import qualified Memoria.View.EditAnswer as V

data FormResult =
    FormResult
        { rAnswer :: Text
        , rIsCorrect :: Text
        }
    deriving (Show)

instance Default FormResult where
    def = FormResult {rAnswer = "", rIsCorrect = "false"}

isCorrectValidator :: Text -> Either Text Text
isCorrectValidator text =
    case text of
        "true" -> Right "true"
        "false" -> Right "false"
        "" -> Right "false"
        _ -> Left $ "Invalid value: " <> text

handleEditAnswer ::
       (C.HasAccounts m, C.HasFooterStats m, C.HasParams m, C.HasRedirects m, C.HasRequestMethod m, DB.HasDb m)
    => m Text
handleEditAnswer = do
    accId <-
        C.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    footerStats <- C.getFooterStats
    answerId <-
        C.getParam "answer-id" >>= \case
            Just aid -> pure aid
            Nothing -> error "answer-id is required"
    answer <-
        DB.getAnswerById accId answerId >>= \case
            Just ans -> pure ans
            Nothing -> error "No such answer"
    question <- DB.getQuestionById (DB.ansQuestionId answer)
    method <- C.getRequestMethod
    case method of
        "GET" ->
            pure $
            V.renderEditAnswer
                footerStats
                V.EditAnswerFormData
                    { V.eaAnswer = DB.ansAnswer answer
                    , V.eaAnswerErr = Nothing
                    , V.eaAnswerId = DB.ansId answer
                    , V.eaIsCorrect =
                          case DB.ansIsCorrect answer of
                              True -> "true"
                              False -> "false"
                    , V.eaIsCorrectErr = Nothing
                    , V.eaQuestion = DB.qQuestion question
                    }
        "POST" -> do
            answer <-
                C.getParam "answer" >>= \case
                    Just ans -> pure ans
                    Nothing -> error "answer is required"
            isCorrect <-
                C.getParam "is-correct" >>= \case
                    Just isCorrect -> pure isCorrect
                    Nothing -> pure "false"
            let fields =
                    [ ( "answer"
                      , answer
                      , \_ -> Right answer
                      , \_formData _fieldValue -> _formData {V.eaAnswer = _fieldValue}
                      , \_formData _fieldErr -> _formData {V.eaAnswerErr = Just _fieldErr}
                      , \_formResult _fieldValue -> _formResult {rAnswer = _fieldValue})
                    , ( "is-correct"
                      , isCorrect
                      , isCorrectValidator
                      , \_formData _fieldValue -> _formData {V.eaIsCorrect = _fieldValue}
                      , \_f _e -> _f {V.eaIsCorrectErr = Just _e}
                      , \_r _v ->
                            _r
                                { rIsCorrect =
                                      case _v of
                                          "true" -> "true"
                                          "false" -> "false"
                                          _ -> error "Invalid value"
                                })
                    ]
            case Memoria.Form.processForm fields of
                Left v -> pure $ V.renderEditAnswer footerStats v
                Right (d :: FormResult) -> do
                    liftIO $ fprint ("Memoria.Form.EditAnswer: Got form data: " % shown % "\n") d
                    let newIsCorrect = case rIsCorrect d of
                            "true" -> True
                            _ -> False
                    DB.updateAnswer accId answerId (newIsCorrect, rAnswer d)
                    C.redirect $ "question-answers?question=" <> (DB.qId question)
                    pure "ok"
        _ -> error "Bad method"
