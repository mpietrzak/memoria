{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.EditAnswer (
    EditAnswerFormData(..),
    renderEditAnswer
) where

import Data.Default.Class (Default(def))
import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import qualified Memoria.View.Base as Memoria.View.Base
import Memoria.View.Base (FooterStats)

data EditAnswerFormData = EditAnswerFormData { eaAnswer :: Text
                                             , eaIsCorrect :: Bool }

instance Default EditAnswerFormData where
    def = EditAnswerFormData { eaAnswer = ""
                             , eaIsCorrect = False }

renderEditAnswer :: FooterStats -> EditAnswerFormData -> Text
renderEditAnswer footerStats formData = do
    let content = "xxx"
    Memoria.View.Base.render footerStats content

