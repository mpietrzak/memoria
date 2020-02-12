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

module Memoria.View.Common
    ( humanByteSize
    ) where

import Data.Text.Lazy (Text)
import Formatting ((%), fixed, format, text)

humanByteSizeUnits :: [Text]
humanByteSizeUnits = ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]

humanByteSize :: Integer -> Text
humanByteSize size =
    case humanByteSizeUnits of
        u:us -> _l _sizeDouble u us
        _ -> error "no"
  where
    _l x u us =
        if x < 1000 || us == []
            then (format ((fixed 2) % " " % text) (x :: Double) u)
            else _l (x / 1024) (head us) (tail us)
    _sizeDouble = fromIntegral size :: Double
