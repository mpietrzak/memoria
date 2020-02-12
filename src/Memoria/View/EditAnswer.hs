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

module Memoria.View.EditAnswer
    ( EditAnswerFormData(..)
    , renderEditAnswer
    ) where

import Data.Default.Class (Default(def))
import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!), (!?))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base as Memoria.View.Base
import Memoria.View.Base (FooterStats)

-- TODO: Question, answerId are not form data.
data EditAnswerFormData =
    EditAnswerFormData
        { eaAnswer :: Text
        , eaAnswerErr :: Maybe Text
        , eaAnswerId :: Text
        , eaIsCorrect :: Text
        , eaIsCorrectErr :: Maybe Text
        , eaQuestion :: Text
        }

instance Default EditAnswerFormData where
    def =
        EditAnswerFormData
            { eaAnswer = ""
            , eaAnswerErr = Nothing
            , eaIsCorrect = "false"
            , eaIsCorrectErr = Nothing
            , eaQuestion = ""
            }

renderEditAnswer :: FooterStats -> EditAnswerFormData -> Text
renderEditAnswer footerStats formData = do
    let content =
            H.form ! A.method "post" $
            H.table ! A.class_ "form" $
            H.tbody $ do
                H.tr $ do
                    H.td "Answer:"
                    H.td $ H.input ! A.name "answer" ! A.value (H.toValue (eaAnswer formData))
                H.tr $ do
                    H.td "Is correct:"
                    H.td $
                        H.input !? (eaIsCorrect formData == "true", A.checked "checked") !
                        A.name "is-correct" !
                        A.type_ "checkbox" !
                        A.value "true"
                H.tr $
                    H.td ! A.colspan "2" ! A.align "right" $ do
                        H.input ! A.name "answer-id" ! A.type_ "hidden" !
                            A.value (H.toValue (eaAnswerId formData))
                        H.button ! A.type_ "submit" $ "Ok"
    Memoria.View.Base.render footerStats content
