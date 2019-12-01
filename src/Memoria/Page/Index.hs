{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Index (handleIndex) where

import Data.Text.Lazy (Text)

handleIndex :: (Monad m) => m Text
handleIndex = pure "test"

