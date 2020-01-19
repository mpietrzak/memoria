{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Login (handleLogin) where

import Data.Text.Lazy (Text)

import qualified Memoria.View.Login as V

handleLogin :: (Monad m) => m Text
handleLogin = pure $ V.renderLogin

