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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.EditQuestionSet
    ( EditQuestionSetViewData(..)
    , renderEditQuestionSet
    ) where

import Data.Default.Class (Default, def)
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.View.Base (FooterStats)
import qualified Memoria.View.Base

data EditQuestionSetViewData =
    EditQuestionSetViewData
        { evCsrfToken :: Text
        , evQuestionSetId :: Text
        , evQuestionSetName :: Text
        , evQuestionSetNameErr :: Maybe Text
        }

instance Default EditQuestionSetViewData where
    def =
        EditQuestionSetViewData
            { evCsrfToken = ""
            , evQuestionSetId = ""
            , evQuestionSetName = ""
            , evQuestionSetNameErr = Nothing
            }

renderEditQuestionSet :: FooterStats -> EditQuestionSetViewData -> Text
renderEditQuestionSet footerStats viewData = do
    let content =
            H.div $ do
                H.form ! A.method "post" $
                    H.table $
                    H.tbody $ do
                        H.tr $ do
                            H.td "Name:"
                            H.td $
                                H.input ! A.name "name" ! A.type_ "text" !
                                A.value (H.toValue (evQuestionSetName viewData))
                            case evQuestionSetNameErr viewData of
                                Nothing -> ""
                                Just _err -> H.td ! A.class_ "error" $ H.toHtml (_err)
                        H.tr $
                            H.td ! A.align "right" ! A.colspan "2" $ do
                                H.input ! A.name "csrf-token" ! A.type_ "hidden" !
                                    A.value (H.toValue (evCsrfToken viewData))
                                H.input ! A.name "question-set" ! A.type_ "hidden" !
                                    A.value (H.toValue (evQuestionSetId viewData))
                                H.button ! A.type_ "submit" $ "Ok"
    Memoria.View.Base.render footerStats content
