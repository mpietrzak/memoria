{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Test (
    Question(..),
    renderTest
) where

import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!), (!?))
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
                H.form ! A.method "post" $ do
                    H.input
                        ! A.name "answer"
                        ! A.type_ "text"
                    H.button
                        ! A.type_ "submit"
                        $ "Ok"
    Memoria.View.Base.render content footer

