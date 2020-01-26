{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.DeleteQuestionSet (
    QuestionSet(..),
    renderDeleteQuestionSet
) where

import Data.Text.Lazy (Text)
import Data.Default.Class (Default(def))
import Text.Blaze.XHtml1.Strict ((!), (!?))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

data QuestionSet = QuestionSet { qsId :: Text
                               , qsName :: Text
                               , qsCreatedAt :: Text
                               , qsModifiedAt :: Text }

renderDeleteQuestionSet :: Memoria.View.Base.FooterStats -> QuestionSet -> Text
renderDeleteQuestionSet footerStats questionSet = do
    let content = H.div $ do
            H.p $ do
                "Are you sure you want to delete the question set: "
                H.em $ H.toHtml $ (qsName questionSet)
                "?"
                H.br
                "Question set created at "
                H.toHtml $ qsCreatedAt questionSet
                ", and modified at "
                H.toHtml $ qsModifiedAt questionSet
                "."
            H.form ! A.method "post" $ H.button "Confirm"
    Memoria.View.Base.render footerStats content
