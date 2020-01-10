{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.CreateQuestion (
    CreateQuestionFormData(..),
    renderCreateQuestion
) where

import Data.Default.Class (Default(def))
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

data CreateQuestionFormData = CreateQuestionFormData { question :: Maybe Text
                                                     , answer :: Maybe Text
                                                     , questionErr :: Maybe Text
                                                     , answerErr :: Maybe Text }

instance Default CreateQuestionFormData where
    def = CreateQuestionFormData { question = Nothing
                                 , answer = Nothing
                                 , questionErr = Nothing
                                 , answerErr = Nothing }

renderCreateQuestion :: Integer -> CreateQuestionFormData -> Text
renderCreateQuestion dbSize formData = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            H.form $ do
                H.table $ do
                    H.tr $ do
                        H.td "Question:"
                        H.td $ do
                            H.input ! A.type_ "text"
                    H.tr $ do
                        H.td "Answer:"
                        H.td $ do
                            H.input ! A.type_ "text"
                    H.tr $ do
                        H.td ! A.align "right" ! A.colspan "3" $ do
                            H.button ! A.type_ "submit" $ do
                                "Ok"
    Memoria.View.Base.render content footer

