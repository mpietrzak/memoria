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

data Answer = Answer { ansId :: Text
                     , ansAnswer :: Text
                     , ansIsCorrect :: Bool
                     , ansAnsweredAt :: Text }

renderQuestionAnswers :: Integer -> [Answer] -> Text
renderQuestionAnswers dbSize answers = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            Memoria.View.Base.menu
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
    Memoria.View.Base.render content footer



