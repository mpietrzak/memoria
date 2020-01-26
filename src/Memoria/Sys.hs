{-# LANGUAGE ForeignFunctionInterface #-}

module Memoria.Sys (
    getCurrentResidentSetSize
) where

import Foreign.C.Types (CSize(..))

foreign import ccall "get_current_resident_set_size" cGetCurrentResidentSetSize :: IO CSize

getCurrentResidentSetSize :: IO Integer
getCurrentResidentSetSize = do
    s <- cGetCurrentResidentSetSize
    pure $ fromIntegral s

