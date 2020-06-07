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

module Memoria.View.QuestionSet
    ( QuestionSet(..)
    , Question(..)
    , renderQuestionSet
    ) where

import Data.Foldable (for_)
import qualified Data.List
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import Formatting (fixed, format)
import Prelude hiding (id)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.View.Base (FooterStats)
import qualified Memoria.View.Base

data QuestionSet =
    QuestionSet
        { qsId :: Text
        , qsName :: Text
        , qsOwnerId :: Text
        , qsCreatedAt :: Text
        , qsModifiedAt :: Text
        , qsIsDeleted :: Bool
        }

data Question =
    Question
        { qId :: Text
        , qQuestion :: Text
        , qAnswer :: Text
        , qScore :: Double
        , qCreatedAt :: Text
        , qModifiedAt :: Text
        }

renderQuestionSet :: FooterStats -> Text -> QuestionSet -> [Question] -> Text
renderQuestionSet footerStats accId questionSet questions = do
    let questionCount = Data.List.length questions
    let averageScore =
            case questionCount of
                0 -> 0.0
                _ -> Data.List.sum (map qScore questions) / (fromIntegral questionCount)
    let content =
            H.div $ do
                H.p $ do
                    "Question set: "
                    H.toHtml $ qsName questionSet
                    H.br
                    "Created: "
                    H.toHtml $ qsCreatedAt questionSet
                    ", modfiied: "
                    H.toHtml $ qsModifiedAt questionSet
                    "."
                    H.br
                    "Average score: "
                    H.toHtml $ format (fixed 2) averageScore
                    "."
                case accId == (qsOwnerId questionSet) of
                    True ->
                        H.div $ do
                            "["
                            H.a ! A.href addQuestionHref $ "Add question"
                            "]"
                            " "
                            "["
                            H.a ! A.href deleteQuestionSetHref $ "Delete question set"
                            "]"
                            " "
                            "["
                            H.a ! A.href editQuestionSetHref $ "Edit question set"
                            "]"
                    False -> ""
                H.table $ do
                    H.thead $
                        H.tr $ do
                            H.th "question, answer"
                            H.th "score"
                            H.th "created, modified"
                    H.tbody $
                        for_ questions $ \q ->
                            H.tr $ do
                                H.td $ do
                                    "Question: "
                                    H.toHtml (qQuestion q)
                                    H.br
                                    "Answer: "
                                    H.toHtml (qAnswer q)
                                H.td $ H.toHtml (format (fixed 2) (qScore q))
                                H.td $ do
                                    H.toHtml (Data.Text.Lazy.take 19 (qCreatedAt q))
                                    ", "
                                    H.toHtml (Data.Text.Lazy.take 19 (qModifiedAt q))
                                H.td $ do
                                    "["
                                    H.a ! A.href (H.toValue ("question-answers?question=" <> qId q)) $
                                        "Show answers"
                                    "]"
                                    "["
                                    H.a ! A.href (H.toValue ("edit-question?question=" <> qId q)) $ "Edit question"
                                    "]"
    Memoria.View.Base.render footerStats content
  where
    addQuestionHref = H.toValue $ "create-question?question-set=" <> qsId questionSet
    deleteQuestionSetHref = H.toValue $ "delete-question-set?question-set=" <> qsId questionSet
    editQuestionSetHref = H.toValue $ "edit-question-set?question-set=" <> qsId questionSet
