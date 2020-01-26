{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Search (
    handleSearch
) where

import Data.Text.Lazy (Text)

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.View.Search as V

handleSearch :: (C.HasAccounts m, C.HasFooterStats m, C.HasParams m, DB.HasDbConn m) => m Text
handleSearch = do
    footerStats <- C.getFooterStats
    query <- C.getParam "q" >>= \case
        Just _q -> pure _q
        Nothing -> error "Query is required"
    accId <- C.getAccountId >>= \case
        Just _accId -> pure _accId
        Nothing -> error "No account id"
    dbSearchResults <- DB.searchQuestionSets accId query
    let vSearchResults = map dbToViewSearchResult dbSearchResults
    pure $ V.renderSearchResults footerStats vSearchResults
    where
        dbToViewSearchResult dbsr = V.SearchResult { V.srName = DB.qsName dbsr }
