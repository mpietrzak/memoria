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
{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.CreateQuestion
    ( CreateQuestionFormData(..)
    , renderCreateQuestion
    ) where

import Data.Default.Class (Default(def))
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!), (!?))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

data CreateQuestionFormData =
    CreateQuestionFormData
        { question :: Maybe Text
        , answer :: Maybe Text
        , questionErr :: Maybe Text
        , answerErr :: Maybe Text
        }
    deriving (Show)

instance Default CreateQuestionFormData where
    def =
        CreateQuestionFormData
            {question = Nothing, answer = Nothing, questionErr = Nothing, answerErr = Nothing}

(!??) h ma =
    case ma of
        Nothing -> h
        Just a -> h ! a

renderCreateQuestion ::
       Memoria.View.Base.FooterStats -> Text -> Text -> CreateQuestionFormData -> Text
renderCreateQuestion stats questionSetId csrfToken formData = do
    let content =
            H.div $ do
                H.p "Adding question"
                H.form ! A.method "post" $
                    H.table $ do
                        H.tr $ do
                            H.td "Question:"
                            H.td $
                                H.input ! A.name "question" ! A.type_ "text" !??
                                case question formData of
                                    Nothing -> Nothing
                                    Just val -> Just $ A.value $ H.toValue val
                            errTd $ questionErr formData
                        H.tr $ do
                            H.td "Answer:"
                            H.td $
                                H.input ! A.name "answer" ! A.type_ "text" !??
                                case answer formData of
                                    Nothing -> Nothing
                                    Just val -> Just $ A.value $ H.toValue val
                            errTd $ answerErr formData
                        H.tr $
                            H.td ! A.align "right" ! A.colspan "2" $ do
                                H.input ! A.name "csrf-token" ! A.type_ "hidden" !
                                    A.value (H.toValue csrfToken)
                                H.input ! A.type_ "hidden" ! A.name "question-set" !
                                    A.value (H.toValue questionSetId)
                                H.button ! A.type_ "submit" $ "Ok"
    Memoria.View.Base.render stats content
  where
    errTd me =
        case me of
            Nothing -> ""
            Just e -> H.td ! A.class_ "error" $ H.toHtml e
