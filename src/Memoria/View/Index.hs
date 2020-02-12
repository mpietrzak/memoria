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

module Memoria.View.Index
    ( QuestionSet(..)
    , renderIndex
    ) where

import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import Prelude hiding (id)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.View.Base (FooterStats)
import qualified Memoria.View.Base

data QuestionSet =
    QuestionSet
        { id :: Text
        , name :: Text
        }

renderIndex :: FooterStats -> [QuestionSet] -> [QuestionSet] -> Text
renderIndex footerStats questionSets subscribedQuestionSets = do
    let content =
            H.div ! A.class_ "content" $ do
                H.div $ do
                    H.p "Search existing question sets:"
                    H.form ! A.method "get" ! A.action "question-set-search" $ do
                        H.input ! A.type_ "text" ! A.name "q"
                        H.button ! A.type_ "submit" $ "Search"
                H.p "Owned question sets:"
                case questionSets of
                    [] ->
                        H.p $ do
                            "You don't own question sets yet..."
                            " "
                            "["
                            H.a ! A.href "create-question-set" $ "Create one"
                            "]"
                    _ -> H.ul $ for_ questionSets questionSetLi
                H.p "Subscribed question sets:"
                case subscribedQuestionSets of
                    [] -> H.p "You don't subscribe question sets yet..."
                    _ -> do
                        H.ul $ for_ subscribedQuestionSets questionSetLi
    Memoria.View.Base.render footerStats content
  where
    questionSetLink _id = H.toValue $ "question-set?id=" <> _id
    questionSetLi questionSet =
        H.li $
        H.a ! A.href (questionSetLink (id questionSet)) $
        H.toHtml $
        case Data.Text.Lazy.strip (name questionSet) of
            "" -> "(unnamed)"
            _ -> (name questionSet)
