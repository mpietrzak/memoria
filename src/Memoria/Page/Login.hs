{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.Login (handleLogin) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text.Lazy (Text)
import Formatting ((%), fprint, int, text)
import qualified Data.List
import qualified Data.Text.Lazy
import qualified Network.Mail.Mime
import qualified Network.Mail.Mime
import qualified Network.Mail.SMTP
import qualified Network.Mail.SMTP.Types
import qualified Network.Socket

import qualified Memoria.Db as DB
import qualified Memoria.Common
import qualified Memoria.View.Login as V

handleLogin :: (Monad m, MonadIO m, DB.HasDb m, Memoria.Common.HasParams m, Memoria.Common.HasRequestMethod m) => m Text
handleLogin = do
    dbSize <- DB.getDbSize >>= \m -> case m of
        Right s -> pure s
        Left _ -> error "Error getting db size"
    method <- Memoria.Common.getRequestMethod
    case method of
        "GET" -> pure $ V.renderLogin dbSize
        "POST" -> do
            mEmail <- Memoria.Common.getParam "email"
            case mEmail of
                Nothing -> error "Email is required"
                Just email -> do
                    accounts <- DB.getAccountsByEmail email
                    liftIO $ fprint
                        ("We have " % int % " accounts\n")
                        (Data.List.length accounts)
                    when ((Data.List.length accounts) > 0) $ do
                        let emailText = makeEmailText accounts
                        liftIO $ fprint
                            ("Auth email to send:\n" % text % "\n")
                            emailText
                        liftIO $ Network.Mail.SMTP.sendMail
                            "localhost"
                            (makeEmail email emailText)
                    pure "Check your inbox"
    where
        makeEmailText accounts = "Click on of the links to..."
        makeEmail to body = Network.Mail.SMTP.simpleMail
            (Network.Mail.SMTP.Types.Address Nothing
                (Data.Text.Lazy.toStrict "memoria@typesafe.org"))
            [(Network.Mail.SMTP.Types.Address Nothing (Data.Text.Lazy.toStrict to))]
            []
            []
            "Memoria login"
            [Network.Mail.Mime.plainPart body]

