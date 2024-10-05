{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module TinyWL.Server where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import TinyWL.FFI (TinyWLServer)


#include "tinywl.h"


type TinyWLServerPtr = Ptr TinyWLServer

-- Getter for wl_display
getWlDisplay :: TinyWLServerPtr -> IO (Ptr ())
getWlDisplay ptr = #{peek struct tinywl_server, wl_display} ptr

-- Getter for renderer
getRenderer :: TinyWLServerPtr -> IO (Ptr ())
getRenderer ptr = #{peek struct tinywl_server, renderer} ptr

-- Getter for backend
getBackend :: TinyWLServerPtr -> IO (Ptr ())
getBackend ptr = #{peek struct tinywl_server, backend} ptr

-- Setter for cursor_mode
setCursorMode :: TinyWLServerPtr -> CInt -> IO ()
setCursorMode ptr mode = #{poke struct tinywl_server, cursor_mode} ptr mode

-- Getter for cursor_mode
getCursorMode :: TinyWLServerPtr -> IO CInt
getCursorMode ptr = #{peek struct tinywl_server, cursor_mode} ptr

-- Add more getters and setters as needed
