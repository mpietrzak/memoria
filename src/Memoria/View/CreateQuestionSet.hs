-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.CreateQuestionSet
    ( CreateQuestionSetFormData(CreateQuestionSetFormData)
    , renderCreateQuestionSet
    , qsfdName
    , qsfdNameError
    ) where

import Data.Text.Lazy (Text)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base
import Memoria.View.Base (FooterStats)

data CreateQuestionSetFormData =
    CreateQuestionSetFormData
        { qsfdName :: Text
        , qsfdNameError :: Maybe Text
        }
    deriving (Show)

renderCreateQuestionSet :: FooterStats -> CreateQuestionSetFormData -> Text
renderCreateQuestionSet footerStats formData = do
    let content =
            H.div $ do
                H.div $
                    H.form ! A.method "post" $
                    H.table $
                    H.tbody $ do
                        H.tr $ do
                            H.td $ H.label "Name:"
                            H.td $ H.input ! A.name "name" ! A.value (H.toValue (qsfdName formData))
                        H.tr $ H.td ! A.colspan "2" ! A.align "right" $ H.button "Create"
    Memoria.View.Base.render footerStats content
