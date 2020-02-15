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

module Memoria.View.Settings
    ( AccountEmail(..)
    , AddEmailFormData(..)
    , NicknameViewData(..)
    , renderDeleteNickname
    , renderSetNickname
    , renderSettings
    , renderSettingsAddEmail
    ) where

import Data.Default.Class (Default(def))
import Data.Foldable (for_)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy
import Text.Blaze.XHtml1.Strict ((!))
import qualified Text.Blaze.XHtml1.Strict as H
import qualified Text.Blaze.XHtml1.Strict.Attributes as A

import Memoria.View.Base (FooterStats)
import qualified Memoria.View.Base

data AccountEmail =
    AccountEmail
        { aeId :: Text
        , aeEmail :: Text
        , aeCreatedAt :: Text
        , aeModifiedAt :: Text
        }

data AddEmailFormData =
    AddEmailFormData
        { aefEmail :: Maybe Text
        , aefEmailErr :: Maybe Text
        }
    deriving (Show)

data NicknameViewData =
    NicknameViewData
        { nvNickname :: Maybe Text
        , nvNicknameErr :: Maybe Text
        }
    deriving (Show)

instance Default AddEmailFormData where
    def = AddEmailFormData {aefEmail = Nothing, aefEmailErr = Nothing}

instance Default NicknameViewData where
    def = NicknameViewData {nvNickname = Nothing, nvNicknameErr = Nothing}

(!??) :: H.Html -> Maybe H.Attribute -> H.Html
(!??) h ma =
    case ma of
        Nothing -> h
        Just a -> h ! a

renderDeleteNickname :: FooterStats -> Text -> Text
renderDeleteNickname footerStats csrfToken = do
    let content =
            H.div $ do
                H.p "Are you sure you want to delete your nickname?"
                H.form ! A.method "post" $ do
                    H.input ! A.name "csrf-token" ! A.type_ "hidden" ! A.value (H.toValue csrfToken)
                    H.button ! A.name "decision" ! A.type_ "submit" ! A.value "yes" $ "Yes"
    Memoria.View.Base.render footerStats content

renderSetNickname :: FooterStats -> NicknameViewData -> Text
renderSetNickname footerStats viewData = do
    let content = do
            case nvNickname viewData of
                Nothing -> H.p "You have no nickname."
                Just nickname ->
                    H.p $ do
                        "Your nickname is: "
                        H.toHtml nickname
            H.div $ do
                H.form ! A.method "post" $
                    H.table ! A.class_ "form" $
                    H.tbody $ do
                        H.tr $ do
                            H.td "New nickname:"
                            H.td $
                                H.input ! A.name "nickname" !??
                                case nvNickname viewData of
                                    Nothing -> Nothing
                                    Just n -> Just $ A.value (H.toValue n)
                        H.tr $ H.td ! A.align "right" ! A.colspan "2" $ H.button "Ok"
    Memoria.View.Base.render footerStats content

renderSettings :: FooterStats -> (Maybe Text) -> [AccountEmail] -> Text
renderSettings footerStats mNickname accountEmails = do
    let content =
            H.div $ do
                H.div $ do
                    "["
                    H.a ! A.href "settings-add-email" $ "Add email"
                    "]"
                case mNickname of
                    Nothing ->
                        H.p $ do
                            "You have no nickname."
                            " "
                            "["
                            H.a ! A.href "set-nickname" $ "Set nickname"
                            "]"
                    Just nickname ->
                        H.p $ do
                            "Your nickname is: "
                            H.em $ H.toHtml nickname
                            " "
                            "["
                            H.a ! A.href "set-nickname" $ "Change nickname"
                            "]"
                            " "
                            "["
                            H.a ! A.href "delete-nickname" $ "Delete nickname"
                            "]"
                case accountEmails of
                    [] -> H.p "You have no emails set. Set some to be able to log in."
                    _ -> do
                        H.h3 "Emails"
                        H.table $ do
                            H.thead $
                                H.tr $ do
                                    H.th "email"
                                    H.th "created at"
                                    H.th "modified at"
                            H.tbody $
                                for_ accountEmails $ \accountEmail ->
                                    H.tr $ do
                                        H.td $ H.toHtml (aeEmail accountEmail)
                                        H.td $
                                            H.toHtml
                                                (Data.Text.Lazy.take 19 (aeCreatedAt accountEmail))
                                        H.td $
                                            H.toHtml
                                                (Data.Text.Lazy.take 19 (aeModifiedAt accountEmail))
    Memoria.View.Base.render footerStats content

renderSettingsAddEmail :: FooterStats -> AddEmailFormData -> Text
renderSettingsAddEmail footerStats formData = do
    let content =
            H.div $ do
                H.form ! A.method "post" $
                    H.table $
                    H.tbody $ do
                        H.tr $ do
                            H.td $
                                H.input ! A.name "email" ! A.type_ "text" !??
                                case aefEmail formData of
                                    Nothing -> Nothing
                                    Just val -> Just $ A.value $ H.toValue val
                            errTd $ aefEmailErr formData
                        H.tr $
                            H.td ! A.align "2" ! A.colspan "2" $ H.button ! A.type_ "submit" $ "Ok"
    Memoria.View.Base.render footerStats content
  where
    errTd me =
        case me of
            Nothing -> ""
            Just e -> H.td ! A.class_ "error" $ H.toHtml e
