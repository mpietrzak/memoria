{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Login (handleLogin) where

import Data.Text.Lazy (Text)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Formatting ((%), fprint, text)

import qualified Memoria.Db as DB
import qualified Memoria.Common
import qualified Memoria.View.Login as V

handleLogin :: (Monad m, MonadIO m, DB.HasDb m, Memoria.Common.HasParams m, Memoria.Common.HasRequestMethod m) => m Text
handleLogin = do
    method <- Memoria.Common.getRequestMethod
    case method of
        "GET" -> pure $ V.renderLogin
        "POST" -> do
            mEmail <- Memoria.Common.getParam "email"
            case mEmail of
                Nothing -> error "Email is required"
                Just email -> do
                    accounts <- DB.getAccountsByEmail email
                    let emailText = makeEmailText accounts
                    liftIO $ fprint
                        ("Auth email to send:\n" % text % "\n")
                        emailText
                    pure "Check your inbox"
    where
        makeEmailText accounts = "Click on of the links to..."
