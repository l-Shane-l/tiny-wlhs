
{-# LANGUAGE ForeignFunctionInterface #-}

module TinyWL.FFI where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array

foreign import ccall "c_main" c_main :: CInt -> Ptr (Ptr CChar) -> IO CInt

withCArgs :: [String] -> IO (Ptr (Ptr CChar))
withCArgs args = newArray =<< mapM newCString args
