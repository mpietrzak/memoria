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

module Memoria.View.Test (
    Question(..),
    renderTest,
    renderTestAnswer
) where

import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A
import qualified Text.Blaze

import Memoria.View.Base (FooterStats)
import qualified Memoria.View.Base

data Question = Question { qId :: Text
                         , qQuestion :: Text
                         , qAnswer :: Text
                         , qCreatedAt :: Text
                         , qModifiedAt :: Text }

renderTest :: FooterStats -> Question -> Text
renderTest footerStats question = do
    let content = H.div $ do
            H.div $ do
                H.p $ do
                    "Question: "
                    H.toHtml (qQuestion question)
                H.form
                    ! A.action "answer"
                    ! A.method "post"
                    $ do
                        H.table ! A.class_ "form" $ H.tbody $ do
                            H.tr $ H.td $ H.textarea
                                ! A.name "answer"
                                ! A.class_ "answer"
                                ! A.cols "60"
                                ! A.rows "5"
                                ! Text.Blaze.customAttribute "autocomplete" "off"
                                $ ""
                            H.tr $ H.td ! A.align "right" $ do
                                H.input
                                    ! A.name "question"
                                    ! A.type_ "hidden"
                                    ! A.value (H.toValue (qId question))
                                H.button ! A.type_ "submit" $ "Ok"
    Memoria.View.Base.render footerStats content

renderTestAnswer :: FooterStats -> (Text, Text) -> Question -> Bool -> Text
renderTestAnswer footerStats (answerId, answer) question isCorrect = do
    let content = H.div $ do
            H.div $ do
                if isCorrect
                    then H.div $ do
                        H.p "This is a correct answer, congrats!"
                        H.p $ do
                            "Question: "
                            H.toHtml (qQuestion question)
                        H.p $ do
                            "Answer: "
                            H.toHtml (qAnswer question)
                    else H.div $ do
                        H.p "This is not a correct answer, sorry..."
                        H.div $ do
                            H.p $ do
                                "Incorrect grade?"
                                " "
                                "["
                                H.a ! A.href "edit-answer" $ "Edit answer"
                                "]"
                            H.p $ do
                                "Incorrect questtion?"
                                " "
                                "["
                                H.a ! A.href "edit-question" $ "Edit question"
                                "]"
                H.div $ do
                    "["
                    H.a ! A.href "test" $ "Next question"
                    "]"

    Memoria.View.Base.render footerStats content

