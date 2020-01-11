{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.CreateQuestion (
    CreateQuestionFormData(..),
    renderCreateQuestion
) where

import Data.Default.Class (Default(def))
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!), (!?))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

data CreateQuestionFormData = CreateQuestionFormData { question :: Maybe Text
                                                     , answer :: Maybe Text
                                                     , questionErr :: Maybe Text
                                                     , answerErr :: Maybe Text }
    deriving Show

instance Default CreateQuestionFormData where
    def = CreateQuestionFormData { question = Nothing
                                 , answer = Nothing
                                 , questionErr = Nothing
                                 , answerErr = Nothing }

(!??) h ma = case ma of
    Nothing -> h
    Just a -> h ! a

renderCreateQuestion :: Integer -> CreateQuestionFormData -> Text
renderCreateQuestion dbSize formData = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            H.p "Adding question"
            H.form $ do
                H.table $ do
                    H.tr $ do
                        H.td "Question:"
                        H.td $ do
                            H.input
                                ! A.name "question"
                                ! A.type_ "text"
                                !?? case (question formData) of
                                        Nothing -> Nothing
                                        Just val -> Just $ A.value $ H.toValue val
                        errTd $ questionErr formData
                    H.tr $ do
                        H.td "Answer:"
                        H.td $ do
                            H.input
                                ! A.name "answer"
                                ! A.type_ "text"
                                !?? case (answer formData) of
                                        Nothing -> Nothing
                                        Just val -> Just $ A.value $ H.toValue val
                        errTd $ answerErr formData
                    H.tr $ do
                        H.td ! A.align "right" ! A.colspan "2" $ do
                            H.button ! A.type_ "submit" $ do
                                "Ok"
    Memoria.View.Base.render content footer

    where
        errTd me = case me of
            Nothing -> ""
            Just e -> H.td ! A.class_ "error" $ do
                H.toHtml e
