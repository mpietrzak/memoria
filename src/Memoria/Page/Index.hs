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

module Memoria.Page.Index
    ( handleIndex
    ) where

import Data.Text.Lazy (Text)
import Prelude hiding (id)

import Memoria.Common (HasAccounts, HasFooterStats)
import qualified Memoria.Common as C
import Memoria.Db (HasDb)
import qualified Memoria.Db as DB
import Memoria.Sessions (HasSessions)
import Memoria.View.Index (renderIndex)
import qualified Memoria.View.Index as V
import Memoria.View.Unauthenticated (renderUnauthenticated)

handleIndex :: (HasAccounts m, HasDb m, HasFooterStats m, HasSessions m, Monad m) => m Text
handleIndex = do
    footerStats <- C.getFooterStats
    mAccountId <- C.getAccountId
    case mAccountId of
        Nothing -> pure $ renderUnauthenticated footerStats
        Just accountId -> do
            dbQuestionSets <- DB.getQuestionSetsForAccount accountId
            dbSubscribedQuestionSets <- DB.getSubscribedQuestionSetsForAccount accountId
            let viewQuestionSets = map qsDbToView dbQuestionSets
            let viewSubscribedQuestionSets = map qsDbToView dbSubscribedQuestionSets
            pure $ renderIndex footerStats viewQuestionSets viewSubscribedQuestionSets
  where
    qsDbToView qs =
        V.QuestionSet
            { V.qsId = DB.qsId qs
            , V.qsName = DB.qsName qs
            , V.qsOwnerNickname = DB.qsOwnerNickname qs
            , V.qsCreatedAt = DB.qsCreatedAt qs
            }
