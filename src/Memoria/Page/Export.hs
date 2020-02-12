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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Export
    ( handleExport
    , handleExportQuestions
    ) where

import Data.Aeson (ToJSON)
import qualified Data.Aeson
import Data.Aeson.TH (defaultOptions, deriveJSON)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import Prelude hiding (id)

import qualified Memoria.Common
import qualified Memoria.Db as DB
import qualified Memoria.View.Export as V

data ExportQuestions =
    ExportQuestions
        { questions :: [ExportQuestion]
        }
    deriving (Generic, Show)

data ExportQuestion =
    ExportQuestion
        { id :: Text
        , question :: Text
        , answer :: Text
        , createdAt :: Text
        , modifiedAt :: Text
        }
    deriving (Show, Generic)

instance ToJSON ExportQuestions

instance ToJSON ExportQuestion

handleExport :: (Memoria.Common.HasAccounts m, Memoria.Common.HasFooterStats m) => m Text
handleExport = do
    footerStats <- Memoria.Common.getFooterStats
    accId <-
        Memoria.Common.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    pure $ V.renderExport footerStats

handleExportQuestions :: (Memoria.Common.HasAccounts m) => m ByteString
handleExportQuestions = do
    accId <-
        Memoria.Common.getAccountId >>= \case
            Just accId -> pure accId
            Nothing -> error "No account id"
    questions <- DB.getAllQuestionsForAccount accId
    let exportQuestionList = map toExportQuestion questions
    let exportQuestionsObject = ExportQuestions {questions = exportQuestionList}
    let questionsJsonBs = Data.Aeson.encode exportQuestionsObject
    pure questionsJsonBs
  where
    toExportQuestion q =
        ExportQuestion
            { id = DB.qId q
            , question = DB.qQuestion q
            , answer = DB.qAnswer q
            , createdAt = DB.qCreatedAt q
            , modifiedAt = DB.qModifiedAt q
            }
