{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Index (renderIndex) where

import Data.Text.Lazy (Text)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base as Memoria.View.Base

renderIndex :: Integer -> Text
renderIndex dbSize = do
    let foot = Memoria.View.Base.footer dbSize
    let content = H.div ! A.class_ "content" $ do
            H.p $ H.toHtml ("hello, world" :: Text)
    Memoria.View.Base.render content foot
