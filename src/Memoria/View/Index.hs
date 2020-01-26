{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Index (
    QuestionSet(..),
    renderIndex
) where

import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Prelude hiding (id)
import Text.Blaze.Html5 ((!))
import qualified Data.Text.Lazy
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.View.Base (FooterStats)
import qualified Memoria.View.Base

data QuestionSet = QuestionSet { id :: Text, name :: Text }

renderIndex :: FooterStats -> [QuestionSet] -> Text
renderIndex footerStats questionSets = do
    let content = H.div ! A.class_ "content" $ do
            H.ul $ for_ questionSets $ \questionSet -> H.li
                $ H.a ! A.href (questionSetLink (id questionSet))
                $ H.toHtml
                $ case Data.Text.Lazy.strip (name questionSet) of
                      "" -> "(unnamed)"
                      _ -> (name questionSet)
    Memoria.View.Base.render footerStats content
    where
        questionSetLink _id = H.toValue $ "question-set?id=" <> _id

