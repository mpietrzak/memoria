{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Formatting ((%), fprint, shown)
import qualified Memoria as Memoria

main :: IO ()
main = do
    result <- Memoria.loadConf "memoria.toml"
    case result of
      Left err -> fprint ("Failed to load config: " % shown % "\n") err
      Right conf -> Memoria.main conf
