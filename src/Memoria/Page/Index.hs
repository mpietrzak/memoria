{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Index (handleIndex) where

import Data.Text.Lazy (Text)

import Memoria.View.Index (renderIndex)
import Memoria.Sessions (HasSessions)
import Memoria.Db (HasDb, getDbSize)

handleIndex :: (HasDb m, HasSessions m, Monad m) => m Text
handleIndex = do
    dbSize <- getDbSize
    pure "test"

