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

import qualified Memoria.View.Base
import Memoria.View.Base (FooterStats)

data CreateQuestionSetFormData = CreateQuestionSetFormData { qsfdName :: Text
                                                           , qsfdNameError :: Maybe Text }
    deriving (Show)

renderCreateQuestionSet :: FooterStats -> CreateQuestionSetFormData -> Text
renderCreateQuestionSet footerStats formData = do
    let content = H.div $ do
            H.div
                $ H.form ! A.method "post"
                $ H.table
                $ H.tbody
                $ do
                    H.tr $ do
                        H.td $ H.label "Name:"
                        H.td $ H.input
                            ! A.name "name"
                            ! A.value (H.toValue (qsfdName formData))
                    H.tr
                        $ H.td ! A.colspan "2" ! A.align "right"
                        $ H.button "Create"
    Memoria.View.Base.render footerStats content

