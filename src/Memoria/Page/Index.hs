{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Index (handleIndex) where

import Data.Text.Lazy (Text)

import Memoria.View.Index (renderIndex)
import Memoria.Sessions (HasSessions)

handleIndex :: (Monad m, HasSessions m) => m Text
handleIndex = do
    pure "test"

