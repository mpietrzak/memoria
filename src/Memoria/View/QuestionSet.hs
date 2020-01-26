{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.QuestionSet (
    QuestionSet(..),
    Question(..),
    renderQuestionSet
) where

import Prelude hiding (id)
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import Data.Foldable (for_)
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.View.Base (FooterStats)
import qualified Memoria.View.Base

data QuestionSet = QuestionSet { id :: Text, name :: Text }

data Question = Question { qId :: Text
                         , qQuestion :: Text
                         , qAnswer :: Text
                         , qCreatedAt :: Text
                         , qModifiedAt :: Text }

renderQuestionSet :: FooterStats -> QuestionSet -> [Question] -> Text
renderQuestionSet footerStats questionSet questions = do
    let content = H.div $ do
            H.div $ do
                "Question set: "
                H.toHtml $ name questionSet
            H.div $ do
                "["
                H.a ! A.href addQuestionHref $ "Add question"
                "]"
            H.table $ do
                H.thead $ H.tr $ do
                    H.th "id"
                    H.th "question"
                    H.th "answer"
                    H.th "created"
                    H.th "modified"
                H.tbody $ for_ questions $ \q -> H.tr $ do
                    H.td $ H.toHtml (qId q)
                    H.td $ H.toHtml (qQuestion q)
                    H.td $ H.toHtml (qAnswer q)
                    H.td $ H.toHtml (qCreatedAt q)
                    H.td $ H.toHtml (qModifiedAt q)
                    H.td $ do
                        "["
                        H.a
                            ! A.href (H.toValue ("question-answers?question=" <> qId q))
                            $ "Show answers"
                        "]"
    Memoria.View.Base.render footerStats content
    where
        addQuestionHref = H.toValue $ "create-question?question-set=" <> id questionSet
