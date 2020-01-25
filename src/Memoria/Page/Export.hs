{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Export (
    handleExport,
    handleExportQuestions
) where

import Prelude hiding (id)
import Data.Aeson (ToJSON)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.ByteString.Lazy (ByteString)
import Data.Text.Lazy (Text)
import GHC.Generics (Generic)
import qualified Data.Aeson

import qualified Memoria.Common
import qualified Memoria.View.Export as V
import qualified Memoria.Db as DB

data ExportQuestions = ExportQuestions { questions :: [ExportQuestion] }
    deriving (Generic, Show)

data ExportQuestion = ExportQuestion { id :: Text
                                     , question :: Text
                                     , answer :: Text
                                     , createdAt :: Text
                                     , modifiedAt :: Text }
    deriving (Show, Generic)

instance ToJSON ExportQuestions
instance ToJSON ExportQuestion

handleExport :: (Memoria.Common.HasAccounts m) => m Text
handleExport = do
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    dbSize <- DB.getDbSize >>= \case
        Right s -> pure s
        Left _ -> error "Error getting db size"
    pure $ V.renderExport dbSize

handleExportQuestions :: (Memoria.Common.HasAccounts m) => m ByteString
handleExportQuestions = do
    accId <- Memoria.Common.getAccountId >>= \case
        Just accId -> pure accId
        Nothing -> error "No account id"
    questions <- DB.getAllQuestionsForAccount accId
    let exportQuestionList = map toExportQuestion questions
    let exportQuestionsObject = ExportQuestions { questions = exportQuestionList }
    let questionsJsonBs = Data.Aeson.encode exportQuestionsObject
    pure questionsJsonBs
    where
        toExportQuestion q = ExportQuestion { id = DB.qId q
                                            , question = DB.qQuestion q
                                            , answer = DB.qAnswer q
                                            , createdAt = DB.qCreatedAt q
                                            , modifiedAt = DB.qModifiedAt q }
