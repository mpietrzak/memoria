{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Settings (
    handleSettings
) where

import Data.Text.Lazy (Text)

import qualified Memoria.View.Settings as V

handleSettings :: (Monad m) => m Text
handleSettings = do
    pure $ V.renderSettings

