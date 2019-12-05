{-# LANGUAGE OverloadedStrings #-}

module Memoria.Common (
    HasAccounts,
    hasAccount
) where

import Data.Text.Lazy (Text)

import Memoria.Sessions (HasSessions, getSessionValue)


sessionAccountIdKey :: Text
sessionAccountIdKey = "account_id"


class HasSessions m => HasAccounts m
    where
        hasAccount :: m Bool
        hasAccount = do
            mAccountId <- getSessionValue sessionAccountIdKey
            case mAccountId of
              Nothing -> pure False
              Just _ -> pure True


