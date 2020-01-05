{-# LANGUAGE OverloadedStrings #-}

module Memoria.Page.CreateQuestion (
    handleCreateQuestion
) where

import Data.Text.Lazy (Text)

handleCreateQuestion :: (Monad m) => m Text
handleCreateQuestion = pure "test"

