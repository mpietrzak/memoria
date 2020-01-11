{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.CreateQuestion (
    handleCreateQuestion
) where

import Formatting ((%), fprint, text, shown)
import Data.Default.Class (def)
import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (liftIO)

import qualified Memoria.Db
import qualified Memoria.View.CreateQuestion as V
import qualified Memoria.Common

validateRequired fieldName val = case val of
    Nothing -> Left $ fieldName <> " is required"
    Just "" -> Left $ fieldName <> " is required"
    Just t -> Right (Just t)

validateQuestion :: Maybe Text -> Either Text (Maybe Text)
validateQuestion = validateRequired "Question"

validateAnswer = validateRequired "Answer"

processField fieldName validator fieldSetter errSetter formData = do
    mval <- Memoria.Common.getParam fieldName
    liftIO $ fprint
        ("processField: Field " % text % " has value " % shown % "\n")
        fieldName
        mval
    let validationResult = validator mval
    case validationResult of
        Left err -> pure $ errSetter err formData
        Right val -> pure $ fieldSetter val formData

handleCreateQuestion :: (Memoria.Common.HasAccounts m, Memoria.Common.HasParams m) => m Text
handleCreateQuestion = do
    accId <- Memoria.Common.getAccountId >>= \m -> case m of
        Just accId -> pure accId
        Nothing -> error "No account id"
    dbSize <- Memoria.Db.getDbSize >>= \m -> case m of
        Right s -> pure s
        Left _ -> error "Error getting db size"
    let fields = [
                (
                    "question",
                    validateQuestion,
                    \v f -> f { V.question = v },
                    \e f -> f { V.questionErr = Just e }
                ),
                (
                    "answer",
                    validateAnswer,
                    \v f -> f { V.answer = v },
                    \e f -> f { V.answerErr = Just e }
                )
             ]
    formData <- processFormData def fields
    liftIO $ fprint
        ("handleCreateQuestion: Form data: " % shown % "\n")
        formData
    pure $ V.renderCreateQuestion dbSize formData

    where
        processFormData formData fields = case fields of
            (f:fs) -> do
                let (name, validator, fieldSetter, errSetter) = f
                newFormData <- processField name validator fieldSetter errSetter formData
                processFormData newFormData fs
            _ -> pure formData

