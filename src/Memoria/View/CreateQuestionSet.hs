{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.CreateQuestionSet
(
    CreateQuestionSetFormData(CreateQuestionSetFormData),
    renderCreateQuestionSet,
    qsfdName,
    qsfdNameError
)
where

import Data.Text.Lazy (Text)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.View.Base (footer)
import qualified Memoria.View.Base
import Memoria.Db (getDbSize)

data CreateQuestionSetFormData = CreateQuestionSetFormData { qsfdName :: Text
                                                           , qsfdNameError :: Maybe Text }
    deriving (Show)

renderCreateQuestionSet :: Integer -> CreateQuestionSetFormData -> Text
renderCreateQuestionSet dbSize formData = do
    let f = footer dbSize
    let content = H.div $ do
            H.form ! A.method "post" $ do
                H.table $ do
                    H.tbody $ do
                        H.tr $ do
                            H.td $ do
                                H.label "Name:"
                            H.td $ do
                                H.input
                                    ! A.name "name"
                                    ! A.value (H.toValue (qsfdName formData))
                        H.tr $ do
                            H.td ! A.colspan "2" ! A.align "right" $ do
                                H.button "Create"
    Memoria.View.Base.render content f

