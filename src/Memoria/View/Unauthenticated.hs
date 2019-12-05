{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Unauthenticated (
    renderUnauthenticated
) where

import Data.Text.Lazy (Text)
import qualified Memoria.View.Base as Memoria.View.Base
import qualified Text.Blaze.XHtml1.Strict as H

renderUnauthenticated :: Text -> Text
renderUnauthenticated dbSize = do
    let foot = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            H.p $ H.toHtml ("hello, unauthenticated" :: Text)
    Memoria.View.Base.render content foot

