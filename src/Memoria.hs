{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}


module Memoria (main) where


import qualified Memoria.Conf as M.Conf
import qualified Web.Scotty.Trans as ST
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.IO.Class (MonadIO)
import Data.Default.Class (def)
import Data.Text.Lazy (Text)

import Memoria.Page.Index (handleIndex)


data State = State


newtype StateM a = StateM
  { runStateM :: ReaderT State IO a
  } deriving (Applicative, Functor, Monad, MonadBase IO, MonadBaseControl IO, MonadIO, MonadReader State)


application :: ST.ScottyT Text StateM ()
application = do
    ST.get "/" $ do
        body <- handleIndex
        ST.html body


main :: M.Conf.Conf -> IO ()
main conf = do
	{-
        pool <- createDbPool
            (cDbHost conf)
            (cDbPort conf)
            (cDbName conf)
            (cDbUser conf)
            (cDbPass conf)
	-}
	let state = State
	let runIO m = runReaderT (runStateM m) state
	ST.scottyOptsT def runIO application
