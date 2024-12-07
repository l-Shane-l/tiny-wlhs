{-# LANGUAGE ForeignFunctionInterface #-}

module LibTinyWLHS.Compositor.FFI where

import Foreign.C.String ()
import Foreign.C.Types (CUInt (..))
import Foreign.Ptr (Ptr)
import LibTinyWLHS.Compositor.Types (
    WlDisplay,
    WlrAllocator,
    WlrBackend,
    WlrCompositor,
    WlrDataDeviceManager,
    WlrOutputLayout,
    WlrRenderer,
    WlrSubCompositor,
 )

-- This should be in wlhs and not here
foreign import ccall "wlr_compositor_create" c_wlr_compositor_create :: Ptr WlDisplay -> CUInt -> Ptr WlrRenderer -> IO (Ptr WlrCompositor)
foreign import ccall "wlr_subcompositor_create" c_wlr_subcompositor_create :: Ptr WlDisplay -> IO (Ptr WlrSubCompositor)
foreign import ccall "wlr_data_device_manager_create" c_wlr_data_device_manager_create :: Ptr WlDisplay -> IO (Ptr WlrDataDeviceManager)
foreign import ccall "wlr_output_layout_create" c_wlr_output_layout_create :: IO (Ptr WlrOutputLayout)
foreign import ccall "wlr_backend_autocreate" c_wlr_backend_autocreate :: Ptr WlDisplay -> Ptr () -> IO (Ptr WlrBackend)
foreign import ccall "wlr_renderer_autocreate" c_wlr_renderer_autocreate :: Ptr WlrBackend -> IO (Ptr WlrRenderer)
foreign import ccall "wlr_renderer_init_wl_display" c_wlr_renderer_init_display :: Ptr WlrRenderer -> Ptr WlDisplay -> IO ()
foreign import ccall "wlr_allocator_autocreate" c_wlr_allocator_autocreate :: Ptr WlrBackend -> Ptr WlrRenderer -> IO (Ptr WlrAllocator)
