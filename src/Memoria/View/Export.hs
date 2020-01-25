{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Export (
    renderExport
) where

import Data.Text.Lazy (Text)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

renderExport :: Integer -> Text
renderExport dbSize = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            Memoria.View.Base.menu
            H.div $ H.p $ H.a
                ! A.href "export-questions"
                $ "Export"
    Memoria.View.Base.render content footer

