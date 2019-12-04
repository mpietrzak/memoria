{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Index (handleIndex) where

import Data.Text.Lazy (Text)
import Formatting ((%), format, shown)

import Memoria.Db (HasDb, getDbSize)
import Memoria.Sessions (HasSessions)
import Memoria.View.Index (renderIndex)

handleIndex :: (HasDb m, HasSessions m, Monad m) => m Text
handleIndex = do
    dbSize <- getDbSize
    case dbSize of
      Left err -> pure $ format ("Error querying DB: " % shown) err
      Right dbSize -> pure $ renderIndex dbSize
