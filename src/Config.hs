module Config where

import Foreign
import Foreign.C (CUInt)
import Foreign.C.String
import System.Environment (getArgs, setEnv)

import qualified LibTinyWLHS.Compositor.Compositor as Compositor
import LibTinyWLHS.Compositor.Types
import LibTinyWLHS.KeyBinding.KeyBindings
import qualified LibTinyWLHS.Server.FFI as FFI
import qualified LibTinyWLHS.Server.Server as Server
import LibTinyWLHS.Server.Types (TinyWLServer)

import Setup
import WLR.Util.Log


data Config = Config
    { logLevel :: WLR_log_importance
    , modKey :: Modifier
    , onStartup :: IO ()
    , onKeyPress :: State -> CUInt -> IO ()
    , startupApplication :: String
    }

data State = State
    { display :: Ptr WlDisplay
    , server :: Ptr TinyWLServer
    }

cycleWindows :: State -> IO Bool
cycleWindows state = FFI.c_cycle_windows $ server state

terminate :: State -> IO ()
terminate state = FFI.c_wl_display_terminate $ display state

customKeybindings
    :: State -> Config -> IO (FunPtr (CUInt -> IO ()))
customKeybindings state config = do
    mkKeybindingHandler $ (onKeyPress config) state

runWLHS :: Config -> IO ()
runWLHS config = do
    wlr_log_init (logLevel config) nullFunPtr
    args <- getArgs
    commandLineArguments args
    setup <- setupServer
    wlr_log WLR_INFO "Server destroyed, shutting down"
    case setup of
        Nothing -> wlr_log WLR_ERROR "Failed to Setup Server"
        Just (myServer, renderer) -> do
            initSuccess <- FFI.c_server_init myServer
            if initSuccess
                then do
                    wlr_log WLR_INFO "Server initialized successfully"
                    wlDisplay <- Server.getWlDisplay myServer
                    _ <- Compositor.initialize_compositor wlDisplay 5 renderer
                    socket <- FFI.c_server_start myServer
                    if socket /= nullPtr
                        then do
                            socketStr <- peekCString socket
                            setEnv "WAYLAND_DISPLAY" socketStr
                            wlr_log WLR_INFO $ "WAYLAND_DISPLAY set to " ++ socketStr
                            withCString (startupApplication config) FFI.c_server_set_startup_command
                            setModKey $ modKey config
                            handlerPtr <- customKeybindings (State { display = wlDisplay, server = myServer} ) config
                            wlr_log WLR_DEBUG $ "Handler created: " ++ show handlerPtr
                            FFI.c_set_keybinding_handler myServer handlerPtr
                            onStartup config
                            wlr_log WLR_INFO "TinyWLHS started"
                            FFI.c_server_run myServer
                        else wlr_log WLR_ERROR "Failed to start server"
                else wlr_log WLR_ERROR "Failed to initialize server"
            FFI.c_server_destroy myServer
            wlr_log WLR_INFO "Server destroyed, shutting down"

