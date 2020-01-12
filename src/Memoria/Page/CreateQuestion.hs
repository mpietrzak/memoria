{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.CreateQuestion (
    handleCreateQuestion
) where

import Control.Monad.IO.Class (liftIO)
import Data.Default.Class (def)
import Data.Text.Lazy (Text)
import Formatting ((%), fprint, text, shown)
import qualified Data.Maybe

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
        Left err -> pure $ (False, errSetter err formData)
        Right val -> pure $ (True, fieldSetter val formData)

handleCreateQuestion :: (Memoria.Common.HasAccounts m, Memoria.Common.HasParams m, Memoria.Common.HasRedirects m, Memoria.Common.HasRequestMethod m) => m Text
handleCreateQuestion = do
    accId <- Memoria.Common.getAccountId >>= \m -> case m of
        Just accId -> pure accId
        Nothing -> error "No account id"
    dbSize <- Memoria.Db.getDbSize >>= \m -> case m of
        Right s -> pure s
        Left _ -> error "Error getting db size"
    questionSetId <- Memoria.Common.getParam "question-set"
        >>= \m -> case m of
            Just id -> pure id
            Nothing -> error "Question set id is required"
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
    method <- Memoria.Common.getRequestMethod
    (isFormDataValid, formData) <- case method of
        "GET" -> pure (False, def)
        _ -> processFormData fields
    case (method, isFormDataValid) of
        ("GET", _) -> pure $ V.renderCreateQuestion dbSize questionSetId formData
        ("POST", False) -> pure $ V.renderCreateQuestion dbSize questionSetId formData
        ("POST", True) -> do
            -- yay?
            Memoria.Db.addQuestion
                accId
                questionSetId
                (
                    (Data.Maybe.fromJust $ V.question formData),
                    (Data.Maybe.fromJust $ V.answer formData))
            Memoria.Common.redirect "/"
            pure ""
    where
        processFormData fields = processFormDataGo True def fields
        processFormDataGo isFormValid formData fields = case fields of
            (f:fs) -> do
                let (name, validator, fieldSetter, errSetter) = f
                (isFieldValid, newFormData) <- processField
                    name
                    validator
                    fieldSetter
                    errSetter
                    formData
                let newIsFormValid = isFieldValid && isFormValid
                processFormDataGo newIsFormValid newFormData fs
            _ -> pure (isFormValid, formData)

