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
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.EditAnswer
    ( handleEditAnswer
    ) where

import Data.Default.Class (def)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.Form
import qualified Memoria.View.EditAnswer as V

handleEditAnswer ::
       (C.HasAccounts m, C.HasFooterStats m, C.HasParams m, C.HasRequestMethod m, DB.HasDb m)
    => m Text
handleEditAnswer = do
    accId <-
        C.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    footerStats <- C.getFooterStats
    answerId <-
        C.getParam "answer" >>= \case
            Just aid -> pure aid
            Nothing -> error "answer is required"
    answer <-
        DB.getAnswerById accId answerId >>= \case
            Just ans -> pure ans
            Nothing -> error "No such answer"
    let fields = []
    method <- C.getRequestMethod
    case method of
        "GET" -> pure $ V.renderEditAnswer footerStats def
        "POST" -> do
            error "x"
        _ -> error "Bad method"
