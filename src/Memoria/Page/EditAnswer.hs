{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.EditAnswer (
    handleEditAnswer
) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.EditAnswer as V

handleEditAnswer :: (Memoria.Common.HasAccounts m, Memoria.Common.HasParams m, DB.HasDb m) => m Text
handleEditAnswer = do
    pure $ V.renderEditAnswer
