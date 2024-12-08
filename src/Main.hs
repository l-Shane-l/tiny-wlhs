module Main where

import Config
import Foreign.C.String
import Foreign.Ptr
import qualified LibTinyWLHS.Compositor.Compositor as Compositor
import LibTinyWLHS.KeyBinding.KeyBindings
import qualified LibTinyWLHS.Server.FFI as FFI
import qualified LibTinyWLHS.Server.Server as Server
import Setup
import System.Environment (getArgs, setEnv)
import WLR.Util.Log

main :: IO ()
main = do
    wlr_log_init (logLevel appConfig) nullFunPtr
    args <- getArgs
    commandLineArguments args
    setup <- setupServer
    case setup of
        Nothing -> wlr_log WLR_ERROR "Failed to Setup Server"
        Just (server, renderer) -> do
            initSuccess <- FFI.c_server_init server
            if initSuccess
                then do
                    wlr_log WLR_INFO "Server initialized successfully"
                    wlDisplay <- Server.getWlDisplay server
                    _ <- Compositor.initialize_compositor wlDisplay 5 renderer
                    socket <- FFI.c_server_start server
                    if socket /= nullPtr
                        then do
                            socketStr <- peekCString socket
                            setEnv "WAYLAND_DISPLAY" socketStr
                            wlr_log WLR_INFO $ "WAYLAND_DISPLAY set to " ++ socketStr
                            withCString (startupApplication appConfig) FFI.c_server_set_startup_command
                            setModKey ModAlt
                            handlerPtr <- customKeybindings wlDisplay server
                            wlr_log WLR_DEBUG $ "Handler created: " ++ show handlerPtr
                            FFI.c_set_keybinding_handler server handlerPtr
                            FFI.c_server_run server
                        else wlr_log WLR_ERROR "Failed to start server"
                else wlr_log WLR_ERROR "Failed to initialize server"
            FFI.c_server_destroy server
            wlr_log WLR_INFO "Server destroyed, shutting down"
