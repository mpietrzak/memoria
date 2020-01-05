{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Unauthenticated (
    renderUnauthenticated
) where

import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base as Memoria.View.Base

renderUnauthenticated :: Integer -> Text
renderUnauthenticated dbSize = do
    let foot = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            H.p $ H.toHtml ("hello, unauthenticated" :: Text)
            H.div $ do
                H.p $ H.toHtml ("Create new account or log in:" :: Text)
                H.a ! A.href "login" $ H.toHtml ("[Login]" :: Text)
                H.toHtml (" " :: Text)
                H.a ! A.href "create-account" $ H.toHtml ("[Create account]" :: Text)
    Memoria.View.Base.render content foot
