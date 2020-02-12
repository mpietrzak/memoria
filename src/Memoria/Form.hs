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

module Memoria.Form
    ( validateRequiredStripped
    , processForm
    ) where

import Data.Default.Class (Default, def)
import qualified Data.Either
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy

validateRequiredStripped :: Text -> Text -> Either Text Text
validateRequiredStripped fieldName value =
    case Data.Text.Lazy.strip value of
        "" -> Left $ fieldName <> " is required"
        t -> Right t

-- |Process a form.
-- Takes:
-- - a list of fields, where each field is:
--   - a field name in HTTP request,
--   - a field value extracted from HTTP request as Text,
--   - field validator which produces either validation error or a valid value,
--   - a setter in the form data,
--   - validation error setter in form data,
--   - a setter in the valid form data.
-- Returns either view form data of invalid form or validated form data.
-- Currently valid form data must be an instance of default :<
processForm ::
       (Data.Default.Class.Default fv, Data.Default.Class.Default fr)
    => [(Text, Text, Text -> Either Text v, fv -> Text -> fv, fv -> Text -> fv, fr -> v -> fr)]
    -> Either fv fr
processForm fields = processForm' True def def fields
  where
    processForm' isValid viewFormData validFormData fields =
        case fields of
            f:fs -> processForm' newIsValid newViewFormData newValidFormData fs
                where (fName, fValue, fValidator, fViewFormDataSetter, fViewFormDataErrSetter, fValidFormdataSetter) =
                          f
                      (newViewFormData, newValidFormData) =
                          case fieldValidationResult of
                              Left fieldValidationErr ->
                                  ( fViewFormDataSetter
                                        (fViewFormDataErrSetter viewFormData fieldValidationErr)
                                        fValue
                                  , validFormData)
                              Right validFieldValue ->
                                  ( fViewFormDataSetter viewFormData fValue
                                  , fValidFormdataSetter validFormData validFieldValue)
                      fieldValidationResult = fValidator fValue
                      isFieldValid = Data.Either.isRight fieldValidationResult
                      newIsValid = isFieldValid && isValid
            _ ->
                case isValid of
                    True -> Right validFormData
                    False -> Left viewFormData
