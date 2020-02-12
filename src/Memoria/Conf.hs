-- Copyright 2020 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
{-# LANGUAGE OverloadedStrings #-}

module Memoria.Conf where

import Data.Default.Class (Default, def)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as Data.Text.Lazy
import qualified Data.Text.Lazy.IO as Data.Text.Lazy.IO
import Formatting ((%), format, shown)
import Toml (TomlCodec, (.=))
import qualified Toml

data Conf =
    Conf
        { cfgDbHost :: Text
        , cfgDbPort :: Int
        , cfgDbName :: Text
        , cfgDbUser :: Text
        , cfgDbPass :: Text
        , cfgPort :: Int
        }
    deriving (Show)

instance Default Conf where
    def =
        Conf
            { cfgDbHost = "localhost"
            , cfgDbPort = 5432
            , cfgDbName = "memoria"
            , cfgDbUser = "memoria"
            , cfgDbPass = "memoria"
            , cfgPort = 9080
            }

confCodec :: TomlCodec Conf
confCodec =
    Conf <$> Toml.lazyText "db.host" .= cfgDbHost <*> Toml.int "db.port" .= cfgDbPort <*>
    Toml.lazyText "db.name" .= cfgDbName <*>
    Toml.lazyText "db.user" .= cfgDbUser <*>
    Toml.lazyText "db.pass" .= cfgDbPass <*>
    Toml.int "web.port" .= cfgPort

load :: Text -> IO (Either Text Conf)
load filename = do
    confSrc <- Data.Text.Lazy.IO.readFile $ Data.Text.Lazy.unpack filename
    let res = Toml.decode confCodec (Data.Text.Lazy.toStrict confSrc)
    case res of
        Left err -> return $ Left (format ("Failed to parse config file: " % shown) err)
        Right c -> return $ Right c
