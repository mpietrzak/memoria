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

{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Memoria.Page.EditQuestion
    ( handleEditQuestion
    ) where

import Data.Default.Class (Default, def)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy

import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.Form as F
import qualified Memoria.View.EditQuestionSet as V

handleEditQuestion = error "not implemented yet"
