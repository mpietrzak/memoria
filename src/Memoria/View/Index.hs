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

data QuestionSet = QuestionSet { id :: Text, name :: Text }

renderIndex :: Integer -> [QuestionSet] -> Text
renderIndex dbSize questionSets = do
    let foot = Memoria.View.Base.footer dbSize
    let content = H.div ! A.class_ "content" $ do
            Memoria.View.Base.menu
            H.ul $ for_ questionSets $ \questionSet -> H.li
                $ H.a ! A.href (questionSetLink (id questionSet))
                $ H.toHtml
                $ name questionSet
    Memoria.View.Base.render content foot
    where
        questionSetLink _id = H.toValue $ "question-set?id=" <> _id
