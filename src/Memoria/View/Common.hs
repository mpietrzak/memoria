{-# LANGUAGE OverloadedStrings #-}

module Memoria.View.Common (
    humanByteSize
) where

import Data.Text.Lazy (Text)
import Formatting ((%), format, fixed, text)

humanByteSizeUnits :: [Text]
humanByteSizeUnits = ["B", "KiB", "MiB", "GiB", "TiB", "PiB", "EiB", "ZiB", "YiB"]

humanByteSize :: Integer -> Text
humanByteSize size = case humanByteSizeUnits of
        u:us -> _l _sizeDouble u us
        _ -> error "no"
    where
        _l x u us = if x < 1000 || us == []
            then (format ((fixed 2) % " " % text) (x::Double) u)
            else _l (x / 1024) (head us) (tail us)
        _sizeDouble = fromIntegral size :: Double