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

module Memoria.View.Login (renderLogin) where

import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base
import Memoria.View.Base (FooterStats)

renderLogin :: FooterStats -> Text
renderLogin footerStats = do
    let content = H.div $ do
            H.p $ H.toHtml ("hello, unauthenticated" :: Text)
            H.div $ H.form ! A.method "post" $ H.table $ H.tbody $ do
                H.tr $ do
                    H.td "Email:"
                    H.td $ H.input ! A.name "email"
                H.tr $ H.td ! A.align "right" ! A.colspan "2" $ H.button
                    ! A.type_ "submit"
                    $ "Ok"
    Memoria.View.Base.renderWithoutMenu footerStats content

