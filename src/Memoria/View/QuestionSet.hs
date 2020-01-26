{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.QuestionSet (
    QuestionSet(..),
    Question(..),
    renderQuestionSet
) where

import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Formatting (format, fixed)
import Prelude hiding (id)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Data.List
import qualified Data.Text.Lazy
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.View.Base (FooterStats)
import qualified Memoria.View.Base

data QuestionSet = QuestionSet { qsId :: Text
                               , qsName :: Text
                               , qsCreatedAt :: Text
                               , qsModifiedAt :: Text }

data Question = Question { qId :: Text
                         , qQuestion :: Text
                         , qAnswer :: Text
                         , qScore :: Double
                         , qCreatedAt :: Text
                         , qModifiedAt :: Text }

renderQuestionSet :: FooterStats -> QuestionSet -> [Question] -> Text
renderQuestionSet footerStats questionSet questions = do
    let questionCount = Data.List.length questions
    let averageScore = case questionCount of
            0 -> 0.0
            _ -> Data.List.sum (map qScore questions) / (fromIntegral questionCount)
    let content = H.div $ do
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
            H.div $ do
                "["
                H.a ! A.href addQuestionHref $ "Add question"
                "]"
                " "
                "["
                H.a ! A.href deleteQuestionSetHref $ "Delete question set"
                "]"
            H.table $ do
                H.thead $ H.tr $ do
                    H.th "question"
                    H.th "answer"
                    H.th "score"
                    H.th "created"
                    H.th "modified"
                H.tbody $ for_ questions $ \q -> H.tr $ do
                    H.td $ H.toHtml (qQuestion q)
                    H.td $ H.toHtml (qAnswer q)
                    H.td $ H.toHtml (format (fixed 2) (qScore q))
                    H.td $ H.toHtml (Data.Text.Lazy.take 19 (qCreatedAt q))
                    H.td $ H.toHtml (Data.Text.Lazy.take 19 (qModifiedAt q))
                    H.td $ do
                        "["
                        H.a
                            ! A.href (H.toValue ("question-answers?question=" <> qId q))
                            $ "Show answers"
                        "]"
    Memoria.View.Base.render footerStats content
    where
        addQuestionHref = H.toValue $ "create-question?question-set=" <> qsId questionSet
        deleteQuestionSetHref = H.toValue $ "delete-question-set?question-set=" <> qsId questionSet

