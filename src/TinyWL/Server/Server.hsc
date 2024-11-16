{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module TinyWL.Server.Server where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
-- the three types below should be imported from wlhs but do not exist there
import TinyWL.Compositor.Types ( WlDisplay, WlrBackend, WlrOutputLayout)
import TinyWL.Server.Types (TinyWLServer)
import WLR.Render.Renderer (WLR_renderer)
#include "tinywl.h"

type TinyWLServerPtr = Ptr TinyWLServer

-- Getter for wl_display
getWlDisplay :: TinyWLServerPtr -> IO (Ptr WlDisplay)
getWlDisplay ptr = #{peek struct tinywl_server, wl_display} ptr

setWlDisplay :: TinyWLServerPtr -> (Ptr WlDisplay) -> IO()
setWlDisplay ptr display = #{poke struct tinywl_server, wl_display} ptr display

-- Getter for renderer
getRenderer :: TinyWLServerPtr -> IO (Ptr WLR_renderer)
getRenderer ptr = #{peek struct tinywl_server, renderer} ptr

-- Getter for backend
getBackend :: TinyWLServerPtr -> IO (Ptr WlrBackend)
getBackend ptr = #{peek struct tinywl_server, backend} ptr

-- Setter for cursor_mode
setCursorMode :: TinyWLServerPtr -> CInt -> IO ()
setCursorMode ptr mode = #{poke struct tinywl_server, cursor_mode} ptr mode

setOutputLayout :: TinyWLServerPtr -> (Ptr WlrOutputLayout) -> IO()
setOutputLayout ptr layout = #{poke struct tinywl_server, output_layout} ptr layout
-- Getter for cursor_mode
getCursorMode :: TinyWLServerPtr -> IO CInt
getCursorMode ptr = #{peek struct tinywl_server, cursor_mode} ptr



