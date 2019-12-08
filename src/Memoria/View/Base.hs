{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Base (footer, render) where

import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import Text.Blaze.Renderer.Text (renderMarkup)
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.Common (humanByteSize)

footer :: Integer -> H.Html
footer dbSize = do
    H.div ! A.class_ "footer" $ do
        H.p $ H.toHtml $ humanByteSize dbSize

render :: H.Html -> H.Html -> Text
render content footer = renderMarkup $ H.docTypeHtml $ do
    H.head $ do
        H.title $ H.toHtml ("test" :: Text)
    H.body $ do
        H.div ! A.class_ "content" $ do
            content
        footer
