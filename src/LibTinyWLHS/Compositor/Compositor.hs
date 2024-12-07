{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module LibTinyWLHS.Compositor.Compositor (
    initialize_compositor,
) where

import Foreign.C.Types
import Foreign.Ptr

import LibTinyWLHS.Compositor.FFI
import LibTinyWLHS.Compositor.Types

initialize_compositor :: Ptr WlDisplay -> CUInt -> Ptr WlrRenderer -> IO (Ptr WlrCompositor, Ptr WlrSubCompositor, Ptr WlrDataDeviceManager)
initialize_compositor wlDisplay num renderer = do
    a <- c_wlr_compositor_create wlDisplay num renderer
    b <- c_wlr_subcompositor_create wlDisplay
    c <- c_wlr_data_device_manager_create wlDisplay
    return (a, b, c)
