{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Index (handleIndex) where

import Data.Text.Lazy (Text)
import Prelude hiding (id)

import Memoria.Common (HasAccounts, HasFooterStats)
import Memoria.Db (HasDb, getQuestionSetsForAccount)
import Memoria.Sessions (HasSessions)
import Memoria.View.Index (renderIndex)
import Memoria.View.Unauthenticated (renderUnauthenticated)
import qualified Memoria.Common
import qualified Memoria.Db
import qualified Memoria.View.Index

handleIndex :: (HasAccounts m, HasDb m, HasFooterStats m, HasSessions m, Monad m) => m Text
handleIndex = do
    footerStats <- Memoria.Common.getFooterStats
    mAccountId <- Memoria.Common.getAccountId
    case mAccountId of
        Nothing -> pure $ renderUnauthenticated footerStats
        Just accountId -> do
            dbQuestionSets <- Memoria.Db.getQuestionSetsForAccount accountId
            let viewQuestionSets = map
                    (\q -> Memoria.View.Index.QuestionSet
                        { Memoria.View.Index.id = Memoria.Db.qsId q
                        , Memoria.View.Index.name = Memoria.Db.qsName q })
                    dbQuestionSets
            pure $ renderIndex footerStats viewQuestionSets

