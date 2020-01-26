{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Export (
    renderExport
) where

import Data.Text.Lazy (Text)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base
import Memoria.View.Base (FooterStats)

renderExport :: FooterStats -> Text
renderExport footerStats = do
    let content = H.div $ do
            H.div $ H.p $ H.a
                ! A.href "export-questions"
                $ "Export"
    Memoria.View.Base.render footerStats content

