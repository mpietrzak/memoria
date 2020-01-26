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
    Memoria.View.Base.render footerStats content

