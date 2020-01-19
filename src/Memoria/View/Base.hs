{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Memoria.View.Base (footer, menu, render) where

import Data.Text.Lazy (Text)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Blaze.XHtml1.Strict ((!))
import Text.RawString.QQ
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.Common (humanByteSize)

css :: Text
css = [r|
        body {
            font-family: sans-serif;
        }

        div.menu {
            margin-bottom: 10px;
            margin-top: 10px;
        }

        div.menu div.menu-item {
            display: inline-block;
            margin-right: 2px;
            width: auto;
        }

        div.footer {
            font-size: 90%;
            margin-bottom: 100px;
            margin-top: 10px;
        }
    |]

footer :: Integer -> H.Html
footer dbSize = do
    H.div ! A.class_ "footer" $ do
        H.p $ do
            "Database size: "
            H.toHtml $ humanByteSize dbSize

menu :: H.Html
menu = do
    H.div ! A.class_ "menu" $ do
        menuItem "/" "index"
        menuItem "create-question-set" "create question set"
        menuItem "test" "test"
        menuItem "settings" "settings"
    where
        menuItem href linkText = do
            H.div ! A.class_ "menu-item" $ do
                "["
                H.a
                    ! A.href href
                    $ linkText
                "]"

render :: H.Html -> H.Html -> Text
render content footer = renderMarkup $ H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml ("test" :: Text)
        H.meta
            ! A.name "viewport"
            ! A.content "width=device-width, initial-scale=1"
        H.style $ H.toHtml css
    H.body $ do
        H.div ! A.class_ "content" $ do
            content
        footer
