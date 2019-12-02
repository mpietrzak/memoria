{-# LANGUAGE OverloadedStrings #-}

module Memoria.Conf where


import Data.Default.Class (Default, def)
import Data.Text.Lazy (Text)


data Conf = Conf { cfgDbHost :: Text
                 , cfgDbPort :: Int
                 , cfgDbName :: Text
                 , cfgDbUser :: Text
                 , cfgDbPass :: Text
                 , cfgPort :: Int }


instance Default Conf where
    def = Conf { cfgDbHost = "localhost"
               , cfgDbPort = 5432
               , cfgDbName = "memoria"
               , cfgDbUser = "memoria"
               , cfgDbPass = "memoria"
               , cfgPort = 9080 }


load :: IO Conf
load = error "Not implemented yet"

