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

module Memoria.View.QuestionSetSubscribe
    ( QuestionSet(..)
    , renderQuestionSetSubscribe
    ) where

import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!), (!?))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

data QuestionSet =
    QuestionSet
        { qsId :: Text
        , qsName :: Text
        , qsOwner :: Text
        , qsCreatedAt :: Text
        , qsModifiedAt :: Text
        }

renderQuestionSetSubscribe :: Memoria.View.Base.FooterStats -> QuestionSet -> Text
renderQuestionSetSubscribe footerStats questionSet = do
    let content =
            H.div $ do
                H.p "Are you sure you want to subscribe to this question set?"
                H.p $ do
                    "Question set name: "
                    H.toHtml (qsName questionSet)
                    "."
                H.form ! A.method "post" $ do
                    H.button "Confirm"
                    H.input ! A.type_ "hidden" ! A.name "question-set" !
                        A.value (H.toValue (qsId questionSet))
    Memoria.View.Base.render footerStats content
