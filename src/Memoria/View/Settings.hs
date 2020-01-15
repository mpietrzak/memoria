{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Settings (
    AccountEmail(..),
    renderSettings,
    renderSettingsAddEmail
) where

import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict.Attributes as A
import qualified Text.Blaze.XHtml1.Strict as H

import qualified Memoria.View.Base

data AccountEmail = AccountEmail { aeId :: Text
                                 , aeEmail :: Text
                                 , aeCreatedAt :: Text
                                 , aeModifiedAt :: Text }

renderSettings :: Integer -> [AccountEmail] -> Text
renderSettings dbSize accountEmails = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            Memoria.View.Base.menu
            H.div $ do
                "["
                H.a
                    ! A.href "settings-add-email"
                    $ do
                        "Add email"
                "]"
            H.table $ do
                H.thead $ do
                    H.tr $ do
                        H.th "email"
                        H.th "created at"
                        H.th "modified at"
                H.tbody $ do
                    for_ accountEmails $ \accountEmail -> do
                        H.tr $ do
                            H.td $ H.toHtml (aeEmail accountEmail)
                            H.td $ H.toHtml (aeCreatedAt accountEmail)
                            H.td $ H.toHtml (aeModifiedAt accountEmail)
    Memoria.View.Base.render content footer

renderSettingsAddEmail :: Integer -> Text
renderSettingsAddEmail dbSize = do
    let footer = Memoria.View.Base.footer dbSize
    let content = H.div $ do
            Memoria.View.Base.menu
            H.table $ do
                H.tbody $ do
                    H.tr $ do
                        H.td $ do
                            H.input
                                ! A.name "email"
                                ! A.type_ "text"
                    H.tr $ do
                        H.td
                            ! A.align "2"
                            ! A.colspan "2"
                            $ do
                                H.button
                                    ! A.type_ "submit"
                                    $ do
                                        "Ok"
    Memoria.View.Base.render content footer
