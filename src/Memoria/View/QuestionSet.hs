{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.QuestionSet (
    QuestionSet(..),
    renderQuestionSet
) where

import Prelude hiding (id)
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

data QuestionSet = QuestionSet { id :: Text, name :: Text }

renderQuestionSet :: Integer -> QuestionSet -> Text
renderQuestionSet dbSize questionSet = do
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
    Memoria.View.Base.render content footer
    where
        addQuestionHref = H.toValue $ "create-question?question-set=" <> (id questionSet)
