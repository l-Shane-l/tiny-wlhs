module Main where

import Control.Monad
import Data.Bits
import Foreign.C.String
import Foreign.Ptr
import System.Environment (getArgs, setEnv)
import System.Process (spawnProcess)
import qualified TinyWL.Compositor.Compositor as Compositor
import qualified TinyWL.Compositor.FFI as FFI
import TinyWL.KeyBindings
import TinyWL.KeySyms
import qualified TinyWL.Server.FFI as FFI
import qualified TinyWL.Server.Server as Server
import WLR.Util.Log

main :: IO ()
main = do
    -- Initialize logging
    wlr_log_init WLR_DEBUG nullFunPtr

    -- Add a test log message
    wlr_log WLR_INFO "Initializing TinyWL with wlhs bindings"

    args <- getArgs
    server <- FFI.c_server_create
    wlr_log WLR_DEBUG "Server created"
    display <- FFI.c_wl_display_create
    _ <- Server.setWlDisplay server display
    backend <- FFI.c_wlr_backend_autocreate display nullPtr
    _ <- Server.setBackend server backend
    renderer <- FFI.c_wlr_renderer_autocreate backend
    _ <- Server.setRenderer server renderer
    _ <- FFI.c_wlr_renderer_init_display renderer display
    allocator <- FFI.c_wlr_allocator_autocreate backend renderer
    _ <- Server.setAllocator server allocator

    outputLayout <- FFI.c_wlr_output_layout_create
    wlr_log WLR_INFO $ "Created outputLayout pointer: " ++ show outputLayout

    if outputLayout == nullPtr
        then do
            wlr_log WLR_ERROR "Failed to create output layout"
        else do
            wlr_log WLR_INFO $ "About to set outputLayout pointer: " ++ show outputLayout
            _ <- Server.setOutputLayout server outputLayout
            wlr_log WLR_INFO "Output layout set attempt completed"

    layoutPtr <- Server.getOutputLayout server
    wlr_log WLR_INFO $ "Retrieved layoutPtr: " ++ show layoutPtr

    if layoutPtr == nullPtr
        then do
            wlr_log WLR_ERROR "Output layout is null after setting"
            error "Critical: Output layout is null after setting"
        else
            wlr_log WLR_INFO $ "Final output layout pointer: " ++ show layoutPtr
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
                    case args of
                        ("-s" : cmd : _) -> do
                            withCString cmd FFI.c_server_set_startup_command
                            wlr_log WLR_DEBUG $ "Startup command set: " ++ cmd
                        _ -> return ()
                    putStrLn $ "Running Wayland compositor on WAYLAND_DISPLAY=" ++ socketStr
                    setModKey ModAlt
                    let handler sym = do
                            wlr_log WLR_INFO $ "Handler called with sym: " ++ show sym
                            when (sym == keySymToInt KEY_s) $ do
                                wlr_log WLR_INFO "Alt+s pressed, spawning terminal"
                                _ <- spawnProcess "xterm" []
                                pure ()
                            when (sym == keySymToInt KEY_c) $ do
                                wlr_log WLR_INFO "Alt + c pressed closing server"
                                FFI.c_wl_display_terminate wlDisplay
                                pure ()

                    wlr_log WLR_DEBUG "Creating keybinding handler"
                    handlerPtr <- mkKeybindingHandler handler
                    wlr_log WLR_DEBUG $ "Handler created: " ++ show handlerPtr
                    FFI.c_set_keybinding_handler server handlerPtr
                    FFI.c_server_run server
                else wlr_log WLR_ERROR "Failed to start server"
        else wlr_log WLR_ERROR "Failed to initialize server"
    FFI.c_server_destroy server
    wlr_log WLR_INFO "Server destroyed, shutting down"
