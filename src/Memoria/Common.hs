{-# LANGUAGE OverloadedStrings #-}

module Memoria.Common (
    HasAccounts,
    HasParams(getParam),
    HasRedirects(redirect),
    HasRequestMethod,
    getAccountId,
    getRequestMethod,
    hasAccount,
    humanByteSize,
) where

import Data.Text.Lazy (Text)
import Formatting ((%), format, fixed, text)
import qualified Network.HTTP.Types.Method

import Memoria.Sessions (HasSessions, getSessionValue)


sessionAccountIdKey :: Text
sessionAccountIdKey = "account_id"


class HasParams m where
    getParam :: Text -> m (Maybe Text)

class HasRedirects m where
    redirect :: Text -> m ()

class HasRequestMethod m where
    getRequestMethod :: m Network.HTTP.Types.Method.Method

class HasSessions m => HasAccounts m
    where
        getAccountId :: m (Maybe Text)
        getAccountId = do
            mAccountId <- getSessionValue sessionAccountIdKey
            pure $ mAccountId
        hasAccount :: m Bool
        hasAccount = do
            mAccountId <- getSessionValue sessionAccountIdKey
            case mAccountId of
              Nothing -> pure False
              Just _ -> pure True

humanByteSizeUnits :: [Text]
humanByteSizeUnits = ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]

humanByteSize :: Integer -> Text
humanByteSize size = case humanByteSizeUnits of
        u:us -> _l _sizeDouble u us
        _ -> error "no"
    where
        _l x u us = if x < 1000 || us == []
            then (format ((fixed 2) % " " % text) (x::Double) u)
            else _l (x / 1024) (head us) (tail us)
        _sizeDouble = fromIntegral size :: Double
