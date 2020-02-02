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
{-# LANGUAGE QuasiQuotes #-}

module Memoria.View.Base (
    FooterStats(..),
    render,
    renderWithoutMenu
) where

import Data.Text.Lazy (Text)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Blaze.XHtml1.Strict ((!))
import Text.RawString.QQ
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.View.Common (humanByteSize)

data FooterStats = FooterStats {
    fDatabaseSize :: Maybe Integer,
    fResidentSetSize :: Maybe Integer
}

css :: Text
css = [r|
        body {
            font-family: "Helvetica Neue", "Helvetica", "Roboto", sans-serif;
            font-size: 12pt;
            margin: 0;
            padding: 10px;
        }

        div.menu {
            margin: 0;
            padding: 0;
        }

        div.menu div.menu-item {
            display: inline-block;
            margin-right: 2px;
            width: auto;
        }

        div.footer {
            font-size: 90%;
            margin: 0;
            padding: 0 0 200px 0;
        }

        table.form {
            border-collapse: collapse;
        }

        table.form, table.form > tbody, table.form > tbody > tr, table.form > tbody > tr > td {
            border: 0;
            margin: 0;
            padding: 0;
        }

        textarea.answer {
            box-sizing: border-box;
            margin: 0;
            max-width: 600px;
            width: 100%;
        }
    |]

footer :: FooterStats -> H.Html
footer stats = H.div ! A.class_ "footer" $ H.p $ do
    case fDatabaseSize stats of
        Just dbSize -> do
            "Database size: "
            H.toHtml $ humanByteSize dbSize
            H.br
        Nothing -> ""
    case fResidentSetSize stats of
        Just rss -> do
            "Resident set size: "
            H.toHtml $ humanByteSize rss
            H.br
        Nothing -> ""

menu :: H.Html
menu = H.div ! A.class_ "menu" $ do
    menuItem "/" "Index"
    menuItem "create-question-set" "Create question set"
    menuItem "test" "Test"
    menuItem "settings" "Settings"
    menuItem "export" "Export"
    menuItem "logout" "Logout"
    where
        menuItem href linkText = H.div ! A.class_ "menu-item" $ do
            "["
            H.a
                ! A.href href
                $ linkText
            "]"

-- This is a most common render and should accept things that are unique to
-- each of the most common page.
render :: FooterStats -> H.Html -> Text
render footerStats content = renderMarkup $ H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml ("test" :: Text)
        H.meta
            ! A.name "viewport"
            ! A.content "width=device-width, initial-scale=1"
        H.style $ H.toHtml css
    H.body $ do
        H.div ! A.class_ "menu" $ menu
        H.div ! A.class_ "content" $ content
        footer footerStats


renderWithoutMenu :: FooterStats -> H.Html -> Text
renderWithoutMenu footerStats content = renderMarkup $ H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml ("test" :: Text)
        H.meta
            ! A.name "viewport"
            ! A.content "width=device-width, initial-scale=1"
        H.style $ H.toHtml css
    H.body $ do
        H.div ! A.class_ "content" $ content
        footer footerStats

