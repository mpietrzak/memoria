{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Index (
    QuestionSet(..),
    renderIndex
) where

import Prelude hiding (id)
import Data.Text.Lazy (Text)
import Data.Foldable (for_)
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base
import Memoria.View.Base (FooterStats)

data QuestionSet = QuestionSet { id :: Text, name :: Text }

renderIndex :: FooterStats -> [QuestionSet] -> Text
renderIndex footerStats questionSets = do
    let content = H.div ! A.class_ "content" $ do
            H.ul $ for_ questionSets $ \questionSet -> H.li
                $ H.a ! A.href (questionSetLink (id questionSet))
                $ H.toHtml
                $ name questionSet
    Memoria.View.Base.render footerStats content
    where
        questionSetLink _id = H.toValue $ "question-set?id=" <> _id
