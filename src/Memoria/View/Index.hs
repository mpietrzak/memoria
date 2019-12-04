{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Index (renderIndex) where

import Data.Text.Lazy (Text)

import qualified Memoria.View.Base as Memoria.View.Base

renderIndex :: Text -> Text
renderIndex dbSize = do
    let foot = Memoria.View.Base.footer dbSize
    Memoria.View.Base.render foot
