{-# LANGUAGE ForeignFunctionInterface #-}

module LibTinyWLHS.Server.FFI where

import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Array (newArray)
import Foreign.Ptr
import LibTinyWLHS.Compositor.Types (WlDisplay)
import LibTinyWLHS.Server.Types (TinyWLServer)

foreign import ccall "server_create" c_server_create :: IO (Ptr TinyWLServer)

foreign import ccall "server_destroy"
    c_server_destroy :: Ptr TinyWLServer -> IO ()

foreign import ccall "server_init" c_server_init :: Ptr TinyWLServer -> IO Bool

foreign import ccall "server_start"
    c_server_start :: Ptr TinyWLServer -> IO CString

foreign import ccall "server_run" c_server_run :: Ptr TinyWLServer -> IO ()

foreign import ccall "server_set_startup_command"
    c_server_set_startup_command :: CString -> IO ()

foreign import ccall "set_keybinding_handler"
    c_set_keybinding_handler :: Ptr TinyWLServer -> FunPtr (CUInt -> IO ()) -> IO ()

foreign import ccall "cycle_windows"
    c_cycle_windows :: Ptr TinyWLServer -> IO Bool

foreign import ccall "wl_display_terminate"
    c_wl_display_terminate :: Ptr WlDisplay -> IO ()

foreign import ccall "wl_display_create"
    c_wl_display_create :: IO (Ptr WlDisplay)

withCArgs :: [String] -> IO (Ptr (Ptr CChar))
withCArgs args = newArray =<< mapM newCString args
