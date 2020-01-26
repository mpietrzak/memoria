{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Search (
    SearchResult(..),
    renderSearchResults
) where

import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!), (!?))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

data SearchResult = SearchResult { srName :: Text }

renderSearchResults :: Memoria.View.Base.FooterStats -> [SearchResult] -> Text
renderSearchResults footerStats searchResults = do
    let content = H.div $ case searchResults of
            [] -> H.p "Nothing found :<"
            _ -> for_ searchResults renderSearchResult
    Memoria.View.Base.render footerStats content
    where
        renderSearchResult searchResult = H.p $ H.a $ H.toHtml (srName searchResult)

