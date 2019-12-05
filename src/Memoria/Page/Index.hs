{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Index (handleIndex) where

import Data.Text.Lazy (Text)
import Formatting ((%), format, shown)

import Memoria.Common (HasAccounts)
import Memoria.Db (HasDb, getDbSize)
import Memoria.Sessions (HasSessions)
import Memoria.View.Index (renderIndex)
import Memoria.View.Unauthenticated (renderUnauthenticated)
import qualified Memoria.Common as Memoria.Common

handleIndex :: (HasAccounts m, HasDb m, HasSessions m, Monad m) => m Text
handleIndex = do
    dbSize <- getDbSize
    case dbSize of
      Left err -> pure $ format ("Error querying DB: " % shown) err
      Right dbSize -> do
          hasAccount <- Memoria.Common.hasAccount
          case hasAccount of
              False -> pure $ renderUnauthenticated dbSize
              True -> do
                pure $ renderIndex dbSize

