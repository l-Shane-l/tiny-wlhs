{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module LibTinyWLHS.Server.Server where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
-- the three types below should be imported from wlhs but do not exist there
import LibTinyWLHS.Compositor.Types ( WlDisplay, WlrBackend, WlrOutputLayout, WlrRenderer, WlrAllocator)
import LibTinyWLHS.Server.Types (TinyWLServer)
#include "tinywl.h"

type TinyWLServerPtr = Ptr TinyWLServer

{-- These functions are currently needed because the structs are implemented in the C in tinwl, In the future I intend to move the structs to wlhs --}

-- Getter for wl_display
getWlDisplay :: TinyWLServerPtr -> IO (Ptr WlDisplay)
getWlDisplay ptr = #{peek struct tinywl_server, wl_display} ptr

setWlDisplay :: TinyWLServerPtr -> (Ptr WlDisplay) -> IO ()
setWlDisplay ptr display = #{poke struct tinywl_server, wl_display} ptr display

-- Getter for renderer
getRenderer :: TinyWLServerPtr -> IO (Ptr WlrRenderer)
getRenderer ptr = #{peek struct tinywl_server, renderer} ptr

setRenderer :: TinyWLServerPtr -> (Ptr WlrRenderer) -> IO () 
setRenderer ptr renderer = #{poke struct tinywl_server, renderer} ptr renderer


-- Getter for backend
getBackend :: TinyWLServerPtr -> IO (Ptr WlrBackend)
getBackend ptr = #{peek struct tinywl_server, backend} ptr

setBackend :: TinyWLServerPtr -> (Ptr WlrBackend) -> IO ()
setBackend ptr backend = #{poke struct tinywl_server, backend} ptr backend

-- Setter for cursor_mode
setCursorMode :: TinyWLServerPtr -> CInt -> IO ()
setCursorMode ptr mode = #{poke struct tinywl_server, cursor_mode} ptr mode

-- Getter for cursor_mode
getCursorMode :: TinyWLServerPtr -> IO CInt
getCursorMode ptr = #{peek struct tinywl_server, cursor_mode} ptr


setOutputLayout :: TinyWLServerPtr -> (Ptr WlrOutputLayout) -> IO ()
setOutputLayout ptr layout = #{poke struct tinywl_server, output_layout} ptr layout

getOutputLayout :: TinyWLServerPtr -> IO (Ptr WlrOutputLayout)
getOutputLayout ptr = #{peek struct tinywl_server, output_layout} ptr

setAllocator :: TinyWLServerPtr -> (Ptr WlrAllocator) -> IO ()
setAllocator ptr allocator = #{poke struct tinywl_server, allocator} ptr allocator
