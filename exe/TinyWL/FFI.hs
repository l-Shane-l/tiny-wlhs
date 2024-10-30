{-# LANGUAGE ForeignFunctionInterface #-}

module TinyWL.FFI where

import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import Foreign.Marshal.Array (newArray)
import WLR.Render.Renderer (WLR_renderer)

data TinyWLServer

-- the five below are temporary and should be imported from wlhs like WLR_renderer is
data WlDisplay
data WlrCompositor
data WlrSubCompositor
data WlrBackend
data WlrDataDeviceManager

-- This should be in wlhs and not here
foreign import ccall "wlr_compositor_create" c_wlr_compositor_create :: Ptr WlDisplay -> CUInt -> Ptr WLR_renderer -> IO (Ptr WlrCompositor)
foreign import ccall "wlr_subcompositor_create" c_wlr_subcompositor_create :: Ptr WlDisplay -> IO (Ptr WlrSubCompositor)
foreign import ccall "wlr_data_device_manager_create" c_wlr_data_device_manager_create :: Ptr WlDisplay -> IO(Ptr WlrDataDeviceManager)

initialize_compositor :: Ptr WlDisplay -> CUInt -> Ptr WLR_renderer -> IO (Ptr WlrCompositor, Ptr WlrSubCompositor, Ptr WlrDataDeviceManager)
initialize_compositor wlDisplay num renderer = do
      a <- c_wlr_compositor_create wlDisplay num renderer
      b <- c_wlr_subcompositor_create wlDisplay
      c <- c_wlr_data_device_manager_create wlDisplay
      return (a,b,c)

foreign import ccall "server_create" c_server_create :: IO (Ptr TinyWLServer)
foreign import ccall "server_destroy" c_server_destroy :: Ptr TinyWLServer -> IO ()
foreign import ccall "server_init" c_server_init :: Ptr TinyWLServer -> IO Bool
foreign import ccall "server_start" c_server_start :: Ptr TinyWLServer -> IO CString
foreign import ccall "server_run" c_server_run :: Ptr TinyWLServer -> IO ()
foreign import ccall "server_set_startup_command" c_server_set_startup_command :: CString -> IO ()



withCArgs :: [String] -> IO (Ptr (Ptr CChar))
withCArgs args = newArray =<< mapM newCString args
