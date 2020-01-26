{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Index (handleIndex) where

import Data.Text.Lazy (Text)
import Prelude hiding (id)

import Memoria.Common (HasAccounts, HasFooterStats)
import Memoria.Db (HasDb)
import Memoria.Sessions (HasSessions)
import Memoria.View.Index (renderIndex)
import Memoria.View.Unauthenticated (renderUnauthenticated)
import qualified Memoria.Common as C
import qualified Memoria.Db as DB
import qualified Memoria.View.Index as V

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
        qsDbToView qs = V.QuestionSet
            { V.id = DB.qsId qs
            , V.name = DB.qsName qs }


