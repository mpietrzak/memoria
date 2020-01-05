{-# LANGUAGE OverloadedStrings #-}

module Memoria.Cookies (
    HasCookies (setCookie, getCookie)
) where

import Data.Text.Lazy (Text)

class Monad m => HasCookies m
    where
        setCookie :: Text -> Text -> m ()
        getCookie :: Text -> m (Maybe Text)


