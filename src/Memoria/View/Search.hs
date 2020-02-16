-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Search
    ( SearchResult(..)
    , renderSearchResults
    ) where

import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base

data SearchResult =
    SearchResult
        { srName :: Text
        , srQuestionSetId :: Text
        , srOwnerNickname :: Maybe Text
        , srCreatedAt :: Text
        }

renderSearchResults :: Memoria.View.Base.FooterStats -> Text -> [SearchResult] -> Text
renderSearchResults footerStats query searchResults = do
    let content = do
            H.div ! A.style "padding-bottom: 16px; padding-top: 16px" $
                H.form ! A.method "get" ! A.action "question-set-search" $ do
                    H.input ! A.type_ "text" ! A.name "q" ! A.value (H.toValue query)
                    H.button ! A.type_ "submit" $ "Search"
            case searchResults of
                [] -> H.p "Nothing found :<"
                _ -> for_ searchResults renderSearchResult
    Memoria.View.Base.render footerStats content
  where
    renderSearchResult searchResult =
        H.div $ do
            H.span $ do
                H.toHtml (srName searchResult)
                ", created by "
                case srOwnerNickname searchResult of
                    Nothing -> "anonymous user"
                    Just nick -> H.toHtml nick
                " at "
                H.toHtml $ Data.Text.Lazy.take 19 $ srCreatedAt searchResult
            " "
            "["
            H.a ! A.href (H.toValue (subscribeSearchResultHref searchResult)) $ "subscribe"
            "]"
    subscribeSearchResultHref searchResult =
        "subscribe-question-set?question-set=" <> (srQuestionSetId searchResult)
