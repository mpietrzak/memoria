{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Settings (
    AccountEmail(..),
    AddEmailFormData(..),
    renderSettings,
    renderSettingsAddEmail
) where

import Data.Default.Class (Default(def))
import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict.Attributes as A
import qualified Text.Blaze.XHtml1.Strict as H

import Memoria.View.Base (FooterStats)
import qualified Memoria.View.Base

data AccountEmail = AccountEmail { aeId :: Text
                                 , aeEmail :: Text
                                 , aeCreatedAt :: Text
                                 , aeModifiedAt :: Text }

data AddEmailFormData = AddEmailFormData { aefEmail :: Maybe Text
                                         , aefEmailErr :: Maybe Text }
    deriving Show

instance Default AddEmailFormData where
    def = AddEmailFormData { aefEmail = Nothing
                           , aefEmailErr = Nothing }

(!??) h ma = case ma of
    Nothing -> h
    Just a -> h ! a

renderSettings :: FooterStats -> [AccountEmail] -> Text
renderSettings footerStats accountEmails = do
    let content = H.div $ do
            H.div $ do
                "["
                H.a
                    ! A.href "settings-add-email"
                    $ "Add email"
                "]"
            H.table $ do
                H.thead $ H.tr $ do
                    H.th "email"
                    H.th "created at"
                    H.th "modified at"
                H.tbody $ for_ accountEmails $ \accountEmail -> H.tr $ do
                    H.td $ H.toHtml (aeEmail accountEmail)
                    H.td $ H.toHtml (aeCreatedAt accountEmail)
                    H.td $ H.toHtml (aeModifiedAt accountEmail)
    Memoria.View.Base.render footerStats content

renderSettingsAddEmail :: FooterStats -> AddEmailFormData -> Text
renderSettingsAddEmail footerStats formData = do
    let content = H.div $ do
            H.form ! A.method "post" $ H.table $ H.tbody $ do
                H.tr $ do
                    H.td $ H.input
                        ! A.name "email"
                        ! A.type_ "text"
                        !?? case aefEmail formData of
                            Nothing -> Nothing
                            Just val -> Just $ A.value $ H.toValue val
                    errTd $ aefEmailErr formData
                H.tr $ H.td
                    ! A.align "2"
                    ! A.colspan "2"
                    $ H.button
                            ! A.type_ "submit"
                            $ "Ok"
    Memoria.View.Base.render footerStats content
    where
        errTd me = case me of
            Nothing -> ""
            Just e -> H.td ! A.class_ "error" $ H.toHtml e
