{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.EditAnswer (
    handleEditAnswer
) where

import Data.Default.Class (def)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.EditAnswer as V

handleEditAnswer :: (Memoria.Common.HasAccounts m, Memoria.Common.HasFooterStats m, Memoria.Common.HasParams m, DB.HasDb m) => m Text
handleEditAnswer = do
    footerStats <- Memoria.Common.getFooterStats
    answerId <- Memoria.Common.getParam "answer"
    let formData = def
    pure $ V.renderEditAnswer footerStats formData
