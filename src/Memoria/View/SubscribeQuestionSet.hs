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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.SubscribeQuestionSet
    ( ViewData(..)
    , renderSubscribeQuestionSet
    ) where

import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

data ViewData =
    ViewData
        { vCsrfToken :: Text
        , vQuestionSetId :: Text
        }

renderSubscribeQuestionSet :: Memoria.View.Base.FooterStats -> ViewData -> Text
renderSubscribeQuestionSet footerStats viewData
    -- TODO: improve html
 = do
    let content = do
            H.div $ do
                "Are you sure you want to subscribe this question set?"
                H.form ! A.method "post" $ do
                    H.input ! A.name "csrf-token" ! A.type_ "hidden" !
                        A.value (H.toValue (vCsrfToken viewData))
                    H.input ! A.name "question-set" ! A.type_ "hidden" !
                        A.value (H.toValue (vQuestionSetId viewData))
                    H.button ! A.name "decision" ! A.type_ "submit" ! A.value "yes" $ "Yes"
    Memoria.View.Base.render footerStats content
