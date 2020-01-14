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

renderCreateQuestion :: Integer -> Text -> Text -> CreateQuestionFormData -> Text
renderCreateQuestion dbSize questionSetId csrfToken formData = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            Memoria.View.Base.menu
            H.p "Adding question"
            H.form ! A.method "post" $ do
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
                            H.input
                                ! A.name "csrf-token"
                                ! A.type_ "hidden"
                                ! (A.value (H.toValue csrfToken))
                            H.input
                                ! A.type_ "hidden"
                                ! A.name "question-set"
                                ! (A.value (H.toValue questionSetId))
                            H.button ! A.type_ "submit" $ do
                                "Ok"
    Memoria.View.Base.render content footer

    where
        errTd me = case me of
            Nothing -> ""
            Just e -> H.td ! A.class_ "error" $ do
                H.toHtml e