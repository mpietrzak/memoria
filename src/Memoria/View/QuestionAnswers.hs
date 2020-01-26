{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.QuestionAnswers (
    Answer(..),
    renderQuestionAnswers
) where

import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base
import Memoria.View.Base (FooterStats)

data Answer = Answer { ansId :: Text
                     , ansAnswer :: Text
                     , ansIsCorrect :: Bool
                     , ansAnsweredAt :: Text }

renderQuestionAnswers :: FooterStats -> [Answer] -> Text
renderQuestionAnswers footerStats answers = do
    let content = H.div $ do
            case answers of
                [] -> H.p "No answers yet"
                _ -> H.table $ do
                    H.thead $ H.tr $ do
                        H.th "Id"
                        H.th "Answer"
                        H.th "Answered at"
                        H.th "Is correct"
                    H.tbody $ for_ answers $ \ans -> H.tr $ do
                        H.td $ H.toHtml $ ansId ans
                        H.td $ H.toHtml $ ansAnswer ans
                        H.td $ H.toHtml $ ansAnsweredAt ans
                        H.td $ H.toHtml $ ansIsCorrect ans
    Memoria.View.Base.render footerStats content



