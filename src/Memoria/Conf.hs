
module Memoria.Conf (Conf(Conf), load) where


import Data.Default.Class (Default, def)


data Conf = Conf { cfgPort :: Int }


instance Default Conf where
    def = Conf { cfgPort = 9080 }


load :: IO Conf
load = error "Not implemented yet"

