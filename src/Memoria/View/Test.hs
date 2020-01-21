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

import qualified Memoria.View.Base

data Question = Question { qId :: Text
                         , qQuestion :: Text
                         , qAnswer :: Text
                         , qCreatedAt :: Text
                         , qModifiedAt :: Text }

renderTest :: Integer -> Question -> Text
renderTest dbSize question = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            Memoria.View.Base.menu
            H.div $ do
                H.p $ do
                    "Question: "
                    H.toHtml (qQuestion question)
                H.form
                    ! A.action "answer"
                    ! A.method "post"
                    $ do
                        H.input ! A.name "answer" ! A.type_ "text"
                        H.input
                            ! A.name "question"
                            ! A.type_ "hidden"
                            ! A.value (H.toValue (qId question))
                        H.button ! A.type_ "submit" $ "Ok"
    Memoria.View.Base.render content footer

renderTestAnswer :: Integer -> (Text, Text) -> Question -> Bool -> Text
renderTestAnswer dbSize (answerId, answer) question isCorrect = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            Memoria.View.Base.menu
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
                                "Incorrect grade? You can override the grade (TODO)"
                            H.p $ do
                                "Incorrect question? You can fix the question (TODO)"
                H.div $ do
                    "["
                    H.a ! A.href "test" $ "Next question"
                    "]"

    Memoria.View.Base.render content footer

