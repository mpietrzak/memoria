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

import qualified Memoria.View.Base

data QuestionSet = QuestionSet { id :: Text, name :: Text }

data Question = Question { qId :: Text
                         , qQuestion :: Text
                         , qAnswer :: Text
                         , qCreatedAt :: Text
                         , qModifiedAt :: Text }

renderQuestionSet :: Integer -> QuestionSet -> [Question] -> Text
renderQuestionSet dbSize questionSet questions = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            Memoria.View.Base.menu
            H.div $ do
                "Question set: "
                H.toHtml $ name questionSet
            H.div $ do
                "["
                H.a ! A.href addQuestionHref $ do
                    "add question"
                "]"
            H.table $ do
                H.thead $ do
                    H.tr $ do
                        H.th "id"
                        H.th "question"
                        H.th "answer"
                        H.th "created"
                        H.th "modified"
                H.tbody $ do
                    for_ questions $ \q -> do
                        H.tr $ do
                            H.td $ H.toHtml (qId q)
                            H.td $ H.toHtml (qQuestion q)
                            H.td $ H.toHtml (qAnswer q)
                            H.td $ H.toHtml (qCreatedAt q)
                            H.td $ H.toHtml (qModifiedAt q)
    Memoria.View.Base.render content footer
    where
        addQuestionHref = H.toValue $ "create-question?question-set=" <> (id questionSet)
